addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x =
  let x = 10; y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y
