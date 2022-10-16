module Identity where

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
