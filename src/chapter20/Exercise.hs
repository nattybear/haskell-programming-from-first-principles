module Exercise where

import Data.Monoid

sum' :: (Foldable t, Num a)
     => t a -> a
sum' xs =
  getSum $ foldMap Sum xs

product' :: (Foldable t, Num a)
         => t a -> a
product' xs =
  getProduct $ foldMap Product xs

elem' :: (Foldable t, Ord a)
      => a -> t a -> Bool
elem' z xs =
  getAny $ foldMap (\x -> Any $ x == z) xs

minimum' :: (Foldable t, Ord a)
         => t a -> Maybe a
minimum' = foldr1' min

maximum' :: (Foldable t, Ord a)
         => t a -> Maybe a
maximum' = foldr1' max

foldr1' :: Foldable t
        => (a -> a -> a) -> t a -> Maybe a
foldr1' f = foldr mf Nothing
  where
    mf x m = Just $ case m of
      Nothing -> x
      Just y  -> f x y

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- | Combine the elements
--   of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr (<>) mempty

-- | Define foldMap in terms of foldr:
foldMap' :: (Foldable t, Monoid m)
         => (a -> m) -> t a -> m
foldMap' f = foldr (\x m -> f x <> m) mempty

-- | Thinking cap time.
--   Write a filter function
--   for Foldable types using
--   the foldMap function:
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f =
  foldMap (\x -> if f x
                 then pure x
                 else mempty)
