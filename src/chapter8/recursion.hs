sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n - 1)

mul :: (Integral a) => a -> a -> a
mul x 1 = x
mul x y = x + mul x (y - 1)
