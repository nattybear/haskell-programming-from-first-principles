module Optional where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a =
    Nada
  | Yep a
  deriving (Eq, Show)

instance Arbitrary a
      => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (1, return Nada)
              , (1, return $ Yep x)
              ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
  pure = Yep
  Nada  <*> _     = Nada
  _     <*> Nada  = Nada
  Yep f <*> Yep x = Yep (f x)

instance Foldable Optional where
  foldr _ z Nada    = z
  foldr f z (Yep x) = f x z

type SI = Sum Int

main :: IO ()
main = do
  let trigger :: Optional (String,String,SI,SI,String)
      trigger = undefined
  quickBatch (foldable trigger)
