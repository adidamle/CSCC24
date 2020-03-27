module Lab08 where

import Lab08Def

-- Implement the following methods for Forky. See the PDF for what to aim for.

instance Functor Forky where
    -- fmap :: (a -> b) -> Forky a -> Forky b
    fmap f (Tip t) = Tip (f t)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Forky where
    -- pure :: a -> Forky a
    pure a = Tip a
    -- (<*>) :: Forky (a -> b) -> Forky a -> Forky b
    (Branch f g) <*> tree = (Branch (f <*> tree) (g <*> tree))
    Tip f <*> tree = fmap f tree