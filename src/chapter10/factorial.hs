factorial' :: [Int]
factorial' = 1 : scanl (*) 1 [2,3..]

factorial :: Int -> Int
factorial n = factorial' !! n
