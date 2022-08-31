module Constant where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a
      => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a
      => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y =
    Constant (x <> y)

instance Foldable (Constant a) where
  foldr f z (Constant _) = z

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

type SI = Sum Int
type TI = Constant SI Int

main :: IO ()
main = do
  let trigger :: Constant String (TI, TI, String, SI)
      trigger = undefined
  quickBatch (traversable trigger)
