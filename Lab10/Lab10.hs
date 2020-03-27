module Lab10 where

import Lab10Def

{-
Lab10Def.hs defines the State monad and a binary tree type.  Please take a look.

You job: Working with the State monad, implement numberHelper below to convert a
binary tree to a new binary tree that has the same shape, but the elements are
Int's, and the numbers go in the order 0, 1, 2,... in the tree's in-order.
There is an example below.

You should not need to maintain your own counter; use the state for the counter.
Also You should not need to write your own "StateOf (\s0 -> ...)" directly; you
should only need put, get, Functor+Applicative+Monad methods, and recursion.
-}

number :: BT v -> BT Int
number t = run (numberHelper t) 0

numberHelper :: BT v -> State Int (BT Int)
numberHelper Null = return Null
numberHelper (Node left _ right) = do
    l <- numberHelper left
    v <- get
    a <- put (v+1)
    r <- numberHelper right
    return (Node l v r)


-- Example: number albertTree = albertTreeNumbered, where:

albertTree :: BT String
albertTree = Node (Node (Node Null "ll" Null)
                        "l"
                        (Node Null "lr" Null))
                  "root"
                  (Node Null
                        "r"
                        (Node Null "rr" Null))

albertTreeNumbered :: BT Int
albertTreeNumbered = Node (Node (Node Null 0 Null)
                                1
                                (Node Null 2 Null))
                          3
                          (Node Null
                                4
                                (Node Null 5 Null))