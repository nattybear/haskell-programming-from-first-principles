import PoemLines

myWords :: String -> [String]
myWords = split ' '

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
