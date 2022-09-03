module ReaderPractice where

import Control.Applicative
import Data.Maybe          hiding (fromMaybe)
import Prelude             hiding (lookup, uncurry)

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((a,b):xs) =
  if x == a
  then Just b
  else lookup x xs

-- zip x and y using 3 as the lookup key
-- Just 6
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
-- Just 9
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- Have x1 make a tuple of
-- xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- write x3, which takes
-- one input and
-- makes a tuple of
-- the results of
-- two applications of
-- z' from above:
x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

-- |
-- >>> x1
-- Just (6,9)
-- >>> x2
-- Nothing
-- >>> x3 3
-- (Just 9,Just 9)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x,y) = f x y

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- we'll make a function similar to some we've seen before
-- that lifts a Boolean function over two partially applied functions:
bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> (<8)

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe _ (Just x) = x

-- |
-- >>> fromMaybe 0 xs
-- 6
-- >>> fromMaybe 0 zs
-- 0

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]

  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7

-- |
-- >>> main
-- Just [3,2,1]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- Just [6,9]
-- Just 15
-- Nothing
-- True
-- [True,False,False]
