curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' = curry' add
