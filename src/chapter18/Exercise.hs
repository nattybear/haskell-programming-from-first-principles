module Exercise where

import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join (fmap f x)

j :: Monad m => m (m a) -> m a
j mm = do
  m <- mm
  m

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = do
  a <- ma
  return (f a)

l2 :: Monad m
   => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = do
  a <- ma
  f <- mf
  return (f a)

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = do
  b  <- f x
  bs <- meh xs f
  return (b:bs)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
