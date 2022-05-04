stops = "pbtdkg"
vowels = "aeiou"

nouns = ["dog", "cat"]
verbs = ["bite", "hug"]

foo :: [(String, String, String)]
foo = [(a,b,c) | a <- nouns, b <- verbs, c <- nouns]

seekritFunc :: String -> Double
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (||) False . map (== x)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x
                             then x : xs
                             else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> f x ++ xs) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> case f x y of
                                    GT -> x
                                    _  -> y) z xs
  where z = last xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> case f x y of
                                    LT -> x
                                    EQ -> x
                                    GT -> y) z xs
  where z = last xs
