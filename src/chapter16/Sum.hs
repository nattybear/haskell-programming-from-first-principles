module Sum where

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

-- |
-- >>> (+1) <$> First 1
-- First 1
-- >>> (+1) <$> Second 1
-- Second 2
