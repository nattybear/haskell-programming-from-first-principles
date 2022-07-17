{-# LANGUAGE FlexibleInstances #-}

module Flip where

import K

newtype Flip f a b = Flip (f b a)
                   deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))
