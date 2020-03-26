module Turbo where

import           Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as Map

import           TurboDef


-- "Run" a Turbo program to produce SVG path commands.
-- This is the only function that will be tested.
runTurbo :: Stmt -> [SVGPathCmd]
runTurbo stmt = snd (deState (turbo stmt) initTurboMem)
-- But the actual execution engine and recursion are in "turbo" and "evalReal"
-- below.

---------------------------------------------------------------------------------------------------

-- Evaluate an expression. The State monad is needed to look up variables.
evalReal :: RealExpr -> State TurboMem Double
evalReal (RLit val) = return val

evalReal (RVar val) = getVar val

evalReal (Neg expr1) = do
      val <- (evalReal expr1)
      let newVal = ((-1) * val)
      return newVal

evalReal (expr1 :+ expr2) = do
      val1 <- (evalReal expr1)
      val2 <- (evalReal expr2)
      let retVal = val1 + val2
      return retVal

evalReal (expr1 :- expr2) = do
      val1 <- (evalReal expr1)
      val2 <- (evalReal expr2)
      let retVal = val1 - val2
      return retVal

evalReal (expr1 :* expr2) = do
      val1 <- (evalReal expr1)
      val2 <- (evalReal expr2)
      let retVal = val1 * val2
      return retVal

evalReal (expr1 :/ expr2) = do
      val1 <- (evalReal expr1)
      val2 <- (evalReal expr2)
      let retVal = val1 / val2
      return retVal

--------------------------------- TURBO HELPERS -------------------------------------------------------

-- This helper function converets polar co-ordinates to a rectangular or the cartesian coordinates.
-- The function takes in a distance (type - double) as the parameter.
-- This distance is used to calculate the rect. coordinates.
polarToRectange :: Double -> State TurboMem SVGPathCmd
polarToRectange dist = do
      -- Get the current angle (direction) and the pen state.
      ang <- getAngle
      penState <- getPen
      -- Since Haskell's library uses radians to calculate the trig angles,
      -- the current angle needs to be converted to radians.
      let radians = (ang/180) * pi
      -- Now convert polar coordinates to the rectangular.
      let deltaX = dist * (cos radians)
      let deltaY = dist * (sin radians)
      -- Based on the pen state, return the desired SVGPathCmd.
      case penState of
            True -> return (LineTo deltaX deltaY)
            False -> return (MoveTo deltaX deltaY)



-- This helper evaluates the SVGpathCmd list for the loops.
helperForLoop :: String -> Double -> Double -> [Stmt] -> State TurboMem [SVGPathCmd]
-- base case: If the for loop Stmt list is empty then 
-- the commands list should be empty too.
helperForLoop _ _ _ [] = return []
-- The recursive case.
-- We stop the recursion when the initial value is greater than the end value.
helperForLoop varName initialVal eVal listOfStatements
            | initialVal > eVal = return []
            | otherwise = do
                  -- Change the var value with each iteration.
                  setVar varName initialVal
                  -- We get the command list by sending in a seq of the stmt list to the turbo
                  -- because the for loop is designed to execute the list as a Seq.
                  l1 <- turbo (Seq listOfStatements)
                  -- Increment the initial value and recurse.
                  l2 <- (helperForLoop varName (initialVal + 1) eVal listOfStatements)
                  return (l1 ++ l2)



-- This helper evaluates the SVGpathCmd list for the Seq (the sequence)
helperForSeq :: Stmt -> State TurboMem [SVGPathCmd]
-- base case: If the Seq has an empty list then 
-- the commands list should be empty too.
helperForSeq (Seq []) = return []
helperForSeq (Seq lst@(x:xs)) = do
      -- get the list of the commands for the 1st item by calling turbo and then 
      -- recurse over the list.
      l1 <- turbo x
      l2 <- turbo (Seq xs)
      -- combine these two lists so that we have a list for the entire sequence.
      return (l1 ++ l2)



----------------------------- MAIN TURBO FUNCTION --------------------------------

-- Run a Turbo statement. Use the State monad to keep state. Return SVG path
-- commands.
-- The implementation of this function is pretty straightforward.
-- Just pattern match and return a list of SVGPathCmd only for the Forward, Seq and For loop.
turbo :: Stmt -> State TurboMem [SVGPathCmd]
turbo (varName := expr) = do
      val <- evalReal expr
      setVar varName val
      return []

turbo PenUp = do
      setPen False
      return []

turbo PenDown = do
      setPen True
      return []

turbo (Turn exp) = do 
      angle <- evalReal exp
      turn angle
      return []

turbo (Forward exp) = do 
      dist <- evalReal exp
      cmd <-polarToRectange dist
      return [cmd]

-- For the seq and the loop call the respective helpers.
turbo (Seq lst) = helperForSeq (Seq lst)

turbo (For varName expr1 expr2 stmtList) = do 
      iVal <- evalReal expr1
      setVar varName iVal
      eVal <- evalReal expr2
      helperForLoop varName iVal eVal stmtList


---------------------------------------------------------------------------------------------------
-- Turbo state:
-- * dictionary of variables->values
-- * current direction (degrees away from x-axis, counterclockwise, e.g.,
--   0 points west, 90 points north)
-- * pen state (True means touching paper)
---------------------- TurboMem (state of variable in form of dictionary) Angle Penstate
data TurboMem = TurboMem (Map String Double) Double Bool
    deriving (Eq, Show)

-- Initial Turbo state: No variables set, direction is 0 degrees, pen is up.
initTurboMem = TurboMem Map.empty 0 False

--------------------------------------- THE SETTERS AND GETTERS ------------------------------------
-- Gets the current direction.
getAngle :: State TurboMem Double
getAngle =  do
       (TurboMem varStates currDirection currPenState) <- get
       return currDirection

-- Changes the direction by adding the given angle.
turn :: Double -> State TurboMem ()
turn angle = modify (\(TurboMem varStates currDir penState) -> (TurboMem varStates (currDir + angle) penState))

-- Gets the pen state.
getPen :: State TurboMem Bool
getPen = do
       (TurboMem varStates currDirection currPenState) <- get
       return currPenState

-- Sets pen state.
setPen :: Bool -> State TurboMem ()
setPen newPenState = modify (\(TurboMem varStates currDir penState) -> (TurboMem varStates currDir newPenState))

-- Gets a variable's current value.
getVar :: String -> State TurboMem Double
getVar varName = do
      (TurboMem varStates currDir penState) <- get
      return (varStates Map.! varName)

-- Sets a variable to value.
setVar :: String -> Double -> State TurboMem ()
setVar varName varValue = 
      modify (\(TurboMem varStates currDir penState) -> (TurboMem (Map.insert varName varValue varStates) currDir penState))