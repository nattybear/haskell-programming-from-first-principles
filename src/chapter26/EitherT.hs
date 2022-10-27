{-# LANGUAGE InstanceSigs #-}

module EitherT where

import Control.Monad
import MonadTrans

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m =>
         Functor (EitherT e m) where
  fmap :: (a -> b)
       -> EitherT e m a
       -> EitherT e m b
  fmap f (EitherT a) =
    EitherT $ (fmap . fmap) f a

instance Applicative m =>
         Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b)
        -> EitherT e m a
        -> EitherT e m b
  EitherT f <*> EitherT a =
    EitherT $ (<*>) <$> f <*> a

instance Monad m =>
         Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a
        -> (a -> EitherT e m b)
        -> EitherT e m b
  EitherT ma >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left x -> return (Left x)
        Right y -> runEitherT (f y)

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . (liftM Right)

swapEither :: Either a b -> Either b a
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT x) =
  EitherT $ swapEither <$> x

eitherT :: Monad m
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g (EitherT mx) = do
  v <- mx
  case v of
    Left x -> f x
    Right y -> g y
