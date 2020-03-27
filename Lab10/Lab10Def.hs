module Lab10Def where

newtype State s a = StateOf (s -> (s,a))

deState :: State s a -> s -> (s, a)
deState (StateOf stf) = stf

-- Like deState but furthermore discards the final state value.
run :: State s a -> s -> a
run prog s0 = snd (deState prog s0)

get :: State s s
get = StateOf (\s0 -> (s0, s0))

put :: s -> State s ()
put s = StateOf (\s0 -> (s, ()))

instance Monad (State s) where
    return a = pure a
    StateOf t >>= k = StateOf (\i0 -> case t i0 of
                                  (i1, a) -> deState (k a) i1)

instance Functor (State s) where
    fmap f (StateOf t) = StateOf (\i0 -> case t i0 of (i1, a) -> (i1, f a))

instance Applicative (State s) where
    pure a = StateOf (\i -> (i, a))
    StateOf tf <*> StateOf ta = StateOf
        (\i0 -> case tf i0 of (i1, f) -> case ta i1 of (i2, a) -> (i2, f a))


data BT v = Null | Node (BT v) v (BT v) deriving Eq

-- This notation goes like: (left-subtree element right-subtree).
-- Example: (((0) 1 (2)) 3 (4 (5)))
instance Show v => Show (BT v) where
    showsPrec _ t = flatsParen t
      where
        flats Null = id
        flats (Node left v right) =
            (case left of Null -> id; t -> flatsParen t . showChar ' ')
            . shows v
            . (case right of Null -> id; t -> showChar ' ' . flatsParen t)
        flatsParen t = showChar '(' . flats t . showChar ')'