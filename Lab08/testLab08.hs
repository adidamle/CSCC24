{-
How to use:

    runghc testLab08.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab08 ()
import Lab08Def

tree1 = Branch (Tip 2) (Branch (Tip 7) (Tip 8))
treeneg1 = Branch (Tip (-2)) (Branch (Tip (-7)) (Tip (-8)))

tests = [ "fmap" ~: fmap negate tree1 ~?= treeneg1
        , "pure" ~: pure 'c' ~?= Tip 'c'
        , "(<*>)"
          ~: Branch (Tip negate) (Tip (+ 1)) <*> tree1
          ~?= Branch treeneg1
                     (Branch (Tip 3) (Branch (Tip 8) (Tip 9)))
        -- more tests when marking
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