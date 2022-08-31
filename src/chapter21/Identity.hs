module Identity where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a
  = Identity a
  deriving (Eq, Ord, Show)

instance Arbitrary a
      => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x
    = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x)
    = f x

instance Traversable Identity where
  traverse f (Identity x)
    = Identity <$> f x

type TI = Identity

main = do
  let trigger :: TI (TI Int, TI Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)
