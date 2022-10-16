module One where

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (One f) where
  fmap f (One fa) = One $ fmap f fa
