module Lab03Def where

data EInteger = NegInf | Fin Integer | PosInf
    deriving (Eq, Show)
-- This models possibly infinite (both +oo and -oo) integers. "E" is for
-- "extended".  NegInf stands for -oo, PosInf stands for +oo, and Fin is the tag
-- for the finite case.