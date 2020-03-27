module Lab07 where

import Lab07Def

{- [6 marks]

   Help me make Weight an instance of Num so I can do basic arithmetic.
   No need to do (-), default implementation uses (+) and negate.
-}
instance Num Weight where
    --(+) :: Weight -> Weight -> Weight
    -- Corner case: anything + Inf = Inf.  The other order too.
    (+) (Fin a) Inf = Inf
    (+) Inf (Fin a) = Inf
    (+) Inf Inf = Inf
    (+) (Fin a) (Fin b) = Fin (a + b)

    --negate :: Weight -> Weight
    -- Corner case done because unsupported:
    negate Inf = error "negative infinity not supported"
    negate (Fin n) = Fin ((-1) * n)

    -- (*) :: Weight -> Weight -> Weight
    -- Corner cases:
    -- positive * Inf = Inf
    -- (zero or negative) * Inf is unsupported
    -- Don't forget the other order.
    (*) Inf Inf = Inf
    (*) (Fin a) (Fin b) = Fin (a * b)
    (*) (Fin a) Inf
        | a <=  0 = error "negative infinity not supported"
        | otherwise = Inf
    (*) Inf (Fin a)
        | a <=  0 = error "negative infinity not supported"
        | otherwise = Inf

    -- abs :: Weight -> Weight
    abs Inf = Inf
    abs (Fin a)
        | a <  0 = (Fin a * (-1))
        | a == 0 = (Fin 0)
        | otherwise = (Fin a)

    -- signum :: Weight -> Weight
    -- Corner case: signum Inf is positive one.
    signum Inf = (Fin 1)
    signum (Fin a)
        | a <  0 = Fin (-1)
        | a == 0 = (Fin 0)
        | otherwise = (Fin 1)

    -- fromInteger :: Integer -> Weight
    fromInteger i = (Fin i) 
    

