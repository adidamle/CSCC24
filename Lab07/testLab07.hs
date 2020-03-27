{-
How to use:

    runghc testLab07.hs
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.CallStack (HasCallStack)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab07Def
import Lab07 ()

tests = [ -- 0.5 marks
          "fin+fin" ~: Fin 47 + Fin (-48) ~?= Fin (-1)
          -- 0.5 marks
        , TestList [ "fin+inf" ~: Fin 4 + Inf ~?= Inf
                   , "inf+fin" ~: Inf + Fin (-4) ~?= Inf
                   , "inf+inf" ~: Inf + Inf ~?= Inf
                   ]
          -- 1 mark
        , "negate fin" ~: negate (Fin (-4)) ~?= Fin 4
          -- 0.5 marks
        , "fin*fin" ~: Fin 7 * Fin (-4) ~?= Fin (-28)
          -- 0.25 marks
        , TestList [ "pos*inf" ~: Fin 4 * Inf ~?= Inf
                   , "inf*pos" ~: Inf * Fin 4 ~?= Inf
                   , "inf*inf" ~: Inf * Inf ~?= Inf
                   ]
          -- 0.25 marks
        , "neg*inf" ~: TestCase (assertError (Fin (-4) * Inf))
          -- 0.5 marks
        , TestList [ "abs pos" ~: abs (Fin 41) ~?= Fin 41
                   , "abs neg" ~: abs (Fin (-41)) ~?= Fin 41
                   ]
          -- 0.5 marks
        , "abs inf" ~: abs Inf ~?= Inf
          -- 1 mark
        , TestList [ "signum pos" ~: signum (Fin 7) ~?= Fin 1
                   , "signum neg" ~: signum (Fin (-7)) ~?= Fin (-1)
                   , "signum 0" ~: signum (Fin 0) ~?= Fin 0
                   , "signum inf" ~: signum Inf ~?= Fin 1
                   ]
          -- 1 mark
        , "fromInteger" ~: fromInteger (-34) ~?= Fin (-34)
        ]

assertError :: HasCallStack => a -> IO ()
assertError a = do
    lr <- try (evaluate a)
    case lr of
      Left (_ :: ErrorCall) -> return ()
      Right _ -> assertFailure ""

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)