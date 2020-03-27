module Lab07Def where

{- I need a number type that supports "positive infinity" for use in
   weighted-graph algorithms such as Prim's, Kruskal's, Dijkstra's.
   Let's say I just need integers and +infinity.
-}
data Weight = Fin Integer | Inf
    deriving (Eq, Ord, Show)

{- With those "deriving"s, I'm almost set. Except:

   Help me make Weight an instance of Num in Lab05.hs so I can do basic
   arithmetic.
-}