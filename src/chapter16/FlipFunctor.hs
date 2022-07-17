{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data Tuple a b = Tuple a b
               deriving (Eq, Show)

newtype Flip f a b = Flip (f b a)
                   deriving (Eq, Show)

instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- | this works, goofy as it looks
-- >>> fmap (+1) (Flip (Tuple 1 "blah"))
-- Flip (Tuple 2 "blah")
