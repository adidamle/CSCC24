-- How to use: runghc testLab03.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab03 (binary, ebinary)
import Lab03Def

tests =
    [ binary 0 ~?= "0"
    , binary 6 ~?= "110"
    , ebinary NegInf ~?= "-inf"
    , ebinary (Fin (-407)) ~?= "-110010111"
    , ebinary (Fin 407) ~?= "110010111"
    , ebinary PosInf ~?= "inf"
    -- more test cases when marking
    ]

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