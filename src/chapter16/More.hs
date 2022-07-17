module More where

data More a b = L a b a
              | R b a b
              deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

-- |
-- >>> fmap (+1) (L 1 2 3)
-- L 1 3 3
-- >>> fmap (+1) (R 1 2 3)
-- R 2 2 4
