module Lab02 where

{-
pow b e computes the exponentiation: b raised to the power e (written b^e
below).  Don't worry about negative e, we will assume e>=0 (make it our
precondition).

Algorithm explanation:

e could be even or odd, i.e., let
  q = e div 2
  r = e mod 2
then e = 2q+r, and r could be 0 or 1.

(In Haskell this can be done with one single "(q, r) = divMod e 2" so the bugs
are elsewhere!)

* If r=0:

    b^e = b^(2q) = (b^q)^2

  so use a recursive call to get b^q, then just square its answer.

* If r=1:

    b^e = b^(2q+1) = b^(2q) * b = (b^q)^2 * b

  so use a recursive call to get b^q, then square its answer, then multiply an
  extra b.

And now we need a base case: b^0 = 1.

My code below contains syntax errors, typos, and bugs.  Fix my code!  But please
don't fix what's right.
-}

pow :: Integer -> Integer -> Integer
pow b e
    | e == 0 = 1
    | r == 1 = y2 * b
    | r == 0 = y2
  where
    (q, r) = divMod e 2
    y = pow b q
    y2 = y * y