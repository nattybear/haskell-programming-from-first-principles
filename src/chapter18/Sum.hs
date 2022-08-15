module Sum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b )
        => Arbitrary (Sum a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [ (1, return $ First x)
              , (1, return $ Second y) ]

instance (Eq a, Eq b)
       => EqProp (Sum a b) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second (f x)

instance Applicative (Sum a) where
  pure = Second

  First x  <*> _        = First x
  _        <*> First y  = First y
  Second f <*> Second x = Second (f x)

instance Monad (Sum a) where
  return = pure

  First x  >>= _ = First x
  Second x >>= f = f x

type SSI = (String,String,Int)

trigger :: Sum String SSI
trigger = undefined

main :: IO ()
main = quickBatch (monad trigger)
