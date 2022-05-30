myIterate :: (a -> a) -> a -> [a]
myIterate f x = y : myIterate f y
  where y = f x
