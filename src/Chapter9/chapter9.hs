import Data.Char
import PoemLines

myWords :: String -> [String]
myWords = split ' '

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Chapter exercises

-- 1. isUpper :: Char -> Bool
--    toUpper :: Char -> Char

-- 2.

upperOnly :: String -> String
upperOnly = filter isUpper

-- 3.

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

-- 4.

capitalize' :: String -> String
capitalize' ""     = ""
capitalize' (x:xs) = toUpper x : capitalize' xs

-- 5. head :: [a] -> a

headFirst :: String -> Char
headFirst = toUpper . head

-- Writing your own standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = any (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xxs) = xs ++ squish xxs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f xs = squish (map f xs)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:x':xs) = case f x x' of
  LT -> myMaximumBy f (x':xs)
  EQ -> myMaximumBy f (x':xs)
  GT -> myMaximumBy f (x :xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:x':xs) = case f x x' of
  LT -> myMinimumBy f (x :xs)
  EQ -> myMinimumBy f (x':xs)
  GT -> myMinimumBy f (x':xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
