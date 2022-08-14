module Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a
  = Pair a
  deriving (Eq, Show)

instance Arbitrary a
      => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary

instance Eq a
      => EqProp (Pair a) where
  (=-=) = eq

instance Functor Pair where
  fmap f (Pair x) = Pair (f x)

instance Applicative Pair where
  pure = Pair

  Pair f <*> Pair x
    = Pair (f x)

type SSI = (String,String,Int)

trigger :: Pair SSI
trigger = undefined

main :: IO ()
main = quickBatch (applicative trigger)
