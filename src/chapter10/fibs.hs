fibs    = take 20 $ 1 : scanl (+) 1 fibs
fibsN x = fibs !! x
