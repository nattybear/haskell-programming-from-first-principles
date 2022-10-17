module Const where

import Bifunctor

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const x) = Const (f x)
