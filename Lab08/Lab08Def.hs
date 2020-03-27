module Lab08Def where

data Forky a = Tip a | Branch (Forky a) (Forky a)
    deriving (Eq, Show)