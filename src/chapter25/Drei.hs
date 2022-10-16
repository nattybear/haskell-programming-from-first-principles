module Drei where

import Bifunctor

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)
