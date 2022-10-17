{-# LANGUAGE InstanceSigs #-}

module IdentityT where

import Control.Monad

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor m =>
         Functor (IdentityT m) where
  fmap :: (a -> b)
       -> IdentityT m a
       -> IdentityT m b
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative m =>
         Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure = IdentityT . pure

  (<*>) :: IdentityT m (a -> b)
        -> IdentityT m a
        -> IdentityT m b
  IdentityT f <*> IdentityT a =
    IdentityT $ f <*> a

instance Monad m =>
         Monad (IdentityT m) where
  return :: a -> IdentityT m a
  return = pure

  (>>=) :: IdentityT m a
        -> (a -> IdentityT m b)
        -> IdentityT m b
  IdentityT ma >>= f =
    IdentityT $ join $ (runIdentityT . f) <$> ma
