{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m =>
         Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT smas) = undefined
