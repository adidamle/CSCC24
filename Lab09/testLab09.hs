{-
How to use:

    runghc testLab09.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import Lab09 ()
import Lab09Def

-- Input and expected answer of handout example.
tree1 = Branch (Tip 2) (Branch (Tip 7) (Tip 8))
k1 i | r == 0 = Tip q
     | otherwise = Branch (Tip q) (Tip (q+1))
  where
    (q, r) = divMod i 2
answer1 = Branch (Tip 1) (Branch (Branch (Tip 3) (Tip 4)) (Tip 4))

-- A large test case.  monster >>= f [] = monsteranswer
monster = Branch (Tip "..B..BB..B..BBB")
                 (Branch (Branch (Tip "..B")
                                 (Branch (Tip ".") (Tip ".")))
                         (Tip "..B..BB"))
f (t2:t1:ts) ('B':cs) = f (Branch t1 t2 : ts) cs
f ts (c:cs) = f (Tip () : ts) cs
f [t] [] = t
f _ _ = error "oops"
monsteranswer = t4
  where
    t4 = Branch t3 t3
    t3 = Branch t2 t2
    t2 = Branch t1 t1
    t1 = Branch tip tip
    tip = Tip ()

tests = [ "handout" ~: (tree1 >>= k1) ~?= answer1
        ]
-- More test cases when marking.

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