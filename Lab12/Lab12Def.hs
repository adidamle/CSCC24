module Lab12Def where

data SExpr = Ident String | List [SExpr] deriving (Eq, Show)