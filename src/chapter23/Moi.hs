{-# LANGUAGE InstanceSigs #-}

module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s ->
    let (x, s') = g s
    in  (f x, s')

-- |
-- >>> f = (+1) <$> (Moi $ \s -> (0, s))
-- >>> runMoi f 0
-- (1,0)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure x = Moi $ \s -> (x, s)

  (<*>) :: Moi s (a -> b)
        -> Moi s a
        -> Moi s b
  Moi f <*> Moi g = Moi $ \s ->
    let (h, s1) = f s
        (x, s2) = g s1
    in  (h x, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  Moi f >>= g = Moi $ \s ->
    let (x, s') = f s
        Moi h   = g x
    in  h s'
