module Lab09 where

import Lab09Def

instance Functor Forky where
    fmap f ta = ta >>= \a -> return (f a)

instance Applicative Forky where
    pure a = return a
    tf <*> ta = tf >>= \f -> ta >>= \a -> pure (f a)

-- Implement Monad's (>>=) for Forky. See the PDF for what to aim for.

instance Monad Forky where
    return :: a -> Forky a

    -- (>>=) :: Forky a -> (a -> Forky b) -> Forky b
    Tip a >>= k = k a
    Branch a b >>= k = Branch (a >>= k) (b >>= k)