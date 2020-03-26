-- How to use: runghc testTurboParser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import ParserLib
import TurboDef
import TurboParser (mainParser)
  
parse :: String -> Maybe Stmt
parse = runParser mainParser

tests =
    [ testHandout
    , testJunkAfter
    ]
-- more test cases when marking

testHandout =
    "handout" ~: parse inp
    ~?= Just (For "i" (RLit 0) (RLit 60)
              [ Forward (RVar "s")
              , "s" := RVar "s" :* RLit 0.99
              , Turn (RVar "i" :* RLit 0.8)
              ])
  where
    inp = "\nfor  i=0 to  60  {\n  forward s;  s   =    s  *  0.99  ;turn   i*0.8;\n  }\n"

testJunkAfter = "junk after" ~: parse "pendown ( " ~?= Nothing

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