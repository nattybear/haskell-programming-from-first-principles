{-# LANGUAGE InstanceSigs  #-}

module StateT where

import MonadTrans

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
                      a <- m
                      return (a, s)

instance Functor m =>
         Functor (StateT s m) where
  fmap :: (a -> b)
       -> StateT s m a
       -> StateT s m b
  fmap f (StateT sma) =
    StateT $ \s ->
      let ma = sma s
      in  fmap (\(a, s') -> (f a, s')) ma

instance Monad m =>
         Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b)
        -> StateT s m a
        -> StateT s m b
  StateT smfs <*> StateT smas =
    StateT $ \s -> do
      (f, s') <- smfs s
      (a, s'') <- smas s'
      return (f a, s'')

instance Monad m =>
         Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a
        -> (a -> StateT s m b)
        -> StateT s m b
  StateT smas >>= f =
    StateT $ \s -> do
      (a, s') <- smas s
      runStateT (f a) s'
