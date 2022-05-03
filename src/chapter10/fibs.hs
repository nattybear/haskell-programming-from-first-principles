fibs    = filter (< 100) $ take 11 $ 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
