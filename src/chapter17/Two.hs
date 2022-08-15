module Two where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b
  = Two a b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b )
      =>   Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

instance (Eq a, Eq b)
      =>  EqProp (Two a b) where
  (=-=) = eq

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a
      => Applicative (Two a) where
  pure x = Two mempty x

  Two m f <*> Two n x
    = Two (m <> n) (f x)

type SSI = (String,String,Int)

trigger :: Two String SSI
trigger = undefined

main :: IO ()
main = quickBatch (applicative trigger)
