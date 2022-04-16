addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x =
  let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \x -> x + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
