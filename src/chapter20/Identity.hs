module Identity where

data Identity a
  = Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z

  foldl f z (Identity x) = f z x

  foldMap f (Identity x) = f x

-- |
-- >>> foldr (*) 1 (Identity 5)
-- 5
-- >>> foldl (*) 5 (Identity 5)
-- 25

-- |
-- >>> import Data.Monoid
-- >>> fm = foldMap (*5)
-- >>> type PI = Product Integer
-- >>> fm (Identity 100) :: PI
-- Product {getProduct = 500}
