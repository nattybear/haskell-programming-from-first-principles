{-# LANGUAGE InstanceSigs #-}

module Deux where

import Bifunctor

data Deux a b = Deux a b

instance Bifunctor Deux where
  first :: (a -> b) -> Deux a c -> Deux b c
  first f (Deux x y) = Deux (f x) y

  second :: (b -> c) -> Deux a b -> Deux a c
  second f (Deux x y) = Deux x (f y)
