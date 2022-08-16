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

instance Eq a
      => EqProp (Identity a) where
  (=-=) = eq

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

main :: IO ()
main = do
  let trigger :: Identity (String, String, Int)
      trigger = undefined
  quickBatch (monad trigger)
