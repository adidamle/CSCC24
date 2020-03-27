module Lab03 where

import Lab03Def

-- [75% marks]
-- 1. Use recursion to convert a non-negative integer to its binary string.
--
-- Precondition: the parameter is >= 0.  No need to worry about negative numbers
-- here.
--
-- You probably want to know: concatenating two strings can be done by the ++
-- infix operator.
--
-- You may discover an annoying corner case.  There are two ways to cope with
-- it.  One way involves a helper function and you put your real recursion
-- there.  Another way doesn't need it.  Both are fine.  (Or if you think
-- "that's not so annoying" that's also fine!)
binary :: Integer -> String
binary num 
    | num == 0 = "0"
    | num == 1 = "1"
    | r == 1 = binary q ++ "1"
    | r == 0 = binary q ++ "0"
  where
    (q, r) = divMod num 2

-- [25% marks]
-- 2. Convert EInteger to binary string. But watch out:
-- For -oo, give the string "-inf"
-- For +oo, give the string "inf"
-- For finite but negative integers, prepend "-"
-- (Don't prepend "+" for +oo or finite positive integers.)
--
-- EInteger is an algebraic data type---recall pattern matching.  Watch out
-- where you absolutely need a pair of parentheses.
--
-- Use binary above to handle finite non-negative integers. (And what can you do
-- to finite negative ones?)
ebinary :: EInteger -> String
ebinary NegInf = "-inf"
ebinary PosInf = "inf"
ebinary (Fin num) 
     | num < 0 = "-" ++ binary (num * (-1))
     | otherwise = binary num
