module Constant where

data Constant a b
  = Constant b
  deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x
