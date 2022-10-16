module Either where

import Bifunctor
import Prelude hiding (Either, Left, Right)

data Either a b =
    Left a
  | Right b

instance Bifunctor Either where
  bimap f _ (Left x) = Left (f x)
  bimap _ g (Right x) = Right (g x)
