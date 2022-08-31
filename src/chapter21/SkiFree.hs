{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a )
        => Arbitrary (S n a) where
  arbitrary
    = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a )
        => EqProp (S n a) where
  (=-=) = eq

instance Functor n
      => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n
      => Foldable (S n) where
  foldMap f (S na a)
    = foldMap f na <> f a

instance Traversable n
      => Traversable (S n) where
  traverse f (S na a)
    = S <$> fb <*> f a
    where
      fb = traverse f na

type SI = Sum Int
type SSI = (String,String,Int)
type Foo = (String,String,SI,SI)

main :: IO ()
main = do
  let trigger :: S [] Foo
      trigger = undefined
  quickBatch (traversable trigger)
