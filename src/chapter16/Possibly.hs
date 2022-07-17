module Possibly where

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers (f x)

-- |
-- >>> (+1) <$> LolNope
-- LolNope
-- >>> (+1) <$> (Yeppers 1)
-- Yeppers 2
