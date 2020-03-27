{-
How to use:

    runghc testLab12.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab12 (sexpr, mainParser)
import Lab12Def
import ParserLib

tests = [ "var" ~: runParser sexpr "v" ~?= Just (Ident "v")
        , "singleton list" ~: runParser sexpr "( p )" ~?= Just (List [Ident "p"])
        , "example" ~:
          runParser sexpr "(  f  ( g  x1 y1)  (h))  "
          ~?= Just (List [ Ident "f"
                         , List [Ident "g",Ident "x1",Ident "y1"]
                         , List [Ident "h"]])
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