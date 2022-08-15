module Three where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c )
        => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

instance (Eq a, Eq b, Eq c)
      =>  EqProp (Three a b c) where
  (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b)
       => Applicative (Three a b) where
  pure x = Three mempty mempty x

  Three m1 m2 f <*> Three n1 n2 x
    = Three (m1 <> n1) (m2 <> n2) (f x)

type SSI = (String,String,Int)

trigger :: Three String String SSI
trigger = undefined

data Three' a b
  = Three' a b b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
       => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance (Eq a, Eq b)
       => EqProp (Three' a b) where
  (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' x y1 y2)
    = Three' x (f y1) (f y2)

instance Monoid a
      => Applicative (Three' a) where
  pure x = Three' mempty x x

  Three' m f g <*> Three' n x y
    = Three' (m <> n) (f x) (g y)

trigger' :: Three' String SSI
trigger' = undefined

main :: IO ()
main = do
  -- quickBatch (applicative trigger)
  quickBatch (applicative trigger')
