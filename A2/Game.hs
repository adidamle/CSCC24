module Game where

import Control.Monad
import Data.Array

import GameDef

------------------------------------------------- QUESTION 1 ------------------------------------------------

-- This function is the implementation of the jugs game.
-- The game finishes without a player prompt when the desired goal is reached even before
-- any player moves.
-- The game is designed to handle situations where the player requests jug(s)
-- which are out of bound of the jugs array. In such situations, the game continues by 
-- showing the correct jugs status.
jugGame :: MonadGame m => Array Int Jug -> Goal -> m ()
jugGame jugArray (Goal jugNum jugAmt)
    | checkGoalJugHasGoalAmt (jugArray!jugNum) jugAmt = return ()
    | otherwise = do
          playerMessage <- gmAction (jugArray) (Goal jugNum jugAmt)
          case playerMessage of
            (FromTo fromJug toJug)
                | (toJug > bound) || (fromJug > bound) || (toJug == fromJug) 
                  || (toJug < 0) || (fromJug < 0) -> jugGame jugArray (Goal jugNum jugAmt)

                -- If goal is achieved, then finish the game.
                | checkW fromJug toJug jugArray jugNum jugAmt -> return ()
                -- Else send the current status to the player and get continue.
                | otherwise -> jugGame (updateJugs fromJug toJug jugArray) (Goal jugNum jugAmt)
            where
              -- Check if the goal is met once the player move is taken into consideration.
              checkW fromJ toJ jugA jugi amt = checkGoalJugHasGoalAmt ((updateJugs fromJ toJ jugA)!jugi) amt
              bound = snd(bounds(jugArray))


-- This helper checks if the given hug meets the given amount.
-- This is made a helper function because it has multiple uses in the assignment.
-- Used in Q1 and Q3.
checkGoalJugHasGoalAmt :: Jug -> Integer -> Bool
checkGoalJugHasGoalAmt (Jug jNum jAmt) desiredJugAmt
    | (jAmt == desiredJugAmt) = True
    | otherwise = False


-- This helper function updates the respective jugs in the inputed jugs array
-- and returns the jugs array with the udpated jugs.
-- To update the jugs, it calls transferBetweenJugs method.
updateJugs :: Int -> Int -> Array Int Jug -> Array Int Jug
updateJugs from to jug = jug // [fst(jugTuple),snd(jugTuple)]
  where
    jugTuple = transferBetweenJugs (from,(jug ! from)) (to,(jug ! to))


-- This helper function transfers the water between the desired jugs
-- by changes the water amounts in the respective jugs.
transferBetweenJugs :: (Int,Jug) -> (Int,Jug) -> ((Int,Jug),(Int,Jug))
transferBetweenJugs (j1, Jug j1Cap j1Amt) (j2, Jug j2Cap j2Amt)
      | ((j2Cap -  j2Amt) >= j1Amt) = ((j1, (Jug j1Cap 0)), (j2, (Jug j2Cap (j1Amt + j2Amt))))
      | otherwise = ((j1, (Jug j1Cap (j1Amt - (j2Cap - j2Amt)))), (j2, (Jug j2Cap j2Cap)))


------------------------------------------------- QUESTION 2 ------------------------------------------------

instance Functor GameTrace where
    fmap = liftM

instance Applicative GameTrace where
    pure = Pure

    (<*>) = ap

instance Monad GameTrace where
    -- return :: a -> GameTrace a
    return = Pure

    -- (>>=) :: GameTrace a -> (a -> GameTrace b) -> GameTrace b
    Pure a >>= k = k a
    Step arr goal next >>= k = Step arr goal (\req -> next req >>= k)

instance MonadGame GameTrace where
    -- gmAction :: Array Int Jug -> Goal -> GameTrace PlayerMsg
    gmAction arr goal = Step arr goal Pure

------------------------------------------------- QUESTION 3 ------------------------------------------------

testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testOneStep game = do
      -- can be changed for other tests
      let from = 2
      let to = 1
      let arr = mkJugArray [Jug 2 0, Jug 3 0, Jug 4 4]
      -- can be Also
      -- updateJ from to arr
      let uparr = mkJugArray [Jug 2 0, Jug 3 3, Jug 4 1]
      let goal = (Goal 0 1)
      testOneStepHelper game from to arr uparr goal


testOneStepHelper :: (Array Int Jug -> Goal -> GameTrace ()) -> Int -> Int -> Array Int Jug -> Array Int Jug -> Goal -> Maybe ()
testOneStepHelper game from to arr uparr goal = go (game arr goal) from to arr uparr goal
    where
    -- check if the it has finished before step
      go (Pure _) from to arr uparr (Goal jugi jugam)
        | (checkGoalJugHasGoalAmt (arr!jugi) jugam) = Just()
        | otherwise = Nothing
      -- if has not finished check everything including array and goal
      go (Step jugb (Goal jugii jugamm) next) from to arr uparr (Goal jugi jugam)
        = checkEverthing jugi jugam (next (FromTo from to)) uparr
      -- check if updated array is in finished state
      checkEverthing jugi jugam (Pure _) uparr
          |(checkGoalJugHasGoalAmt (uparr!jugi) jugam) = Just()
          |otherwise = Nothing
      -- expected array and game produced array match
      checkEverthing jugi jugam (Step updatedarr (Goal jNum jAmt) next) uparr
          |((jugi == jNum) && (jugam == jAmt) && (uparr == updatedarr)) = Just()
          |otherwise = Nothing
      checkIfPure (Pure _) = True
      checkIfPure _ = False

-------------------------------------------------------------------------------------------------

testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testUntilDone gameFunc = do
  -- Initialize the array and the goal.
  let jugsArr = mkJugArray [Jug 2 0, Jug 3 0, Jug 4 4]
  let goal = (Goal 0 1)
  let gt1 = gameFunc jugsArr goal
  -- Check for move 1 - FROM 2 TO 1
  f1 <- checkMove gt1
  let gt2 = f1 (FromTo 2 1)
 -- Check for move 2 - FROM 2 TO 0
  f2 <- checkMove gt2
  let gt3 = f2 (FromTo 2 0)
  -- Since this game ends with just 2 moves, the 3rd GameTrace should be
  -- Pure indicating that the game ends.
  -- If its not Pure at this step or stage then the game isnt correct.
  -- Thus, return failed or pass based on the gt3.
  if ((checkIfPure gt3) == True)
    then
      do
        Just()
  else
    do
      Nothing


-- returns the function if the gameTrace has the function.
checkMove :: GameTrace a -> Maybe (PlayerMsg -> GameTrace a)
checkMove (Pure _) = Nothing
checkMove (Step j g func) = Just func

-- This helper function checks if the gametrace is of type Pure.
checkIfPure :: GameTrace a -> Bool
checkIfPure (Pure _) = True
checkIfPure _ = False