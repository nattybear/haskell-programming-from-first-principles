module Three where

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

main :: IO ()
main = quickBatch (applicative trigger)
