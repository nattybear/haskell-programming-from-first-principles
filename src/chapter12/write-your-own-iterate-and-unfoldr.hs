myIterate :: (a -> a) -> a -> [a]
myIterate f x = y : myIterate f y
  where y = f x

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Nothing     -> []
                  Just (y, z) -> y : myUnfoldr f z

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
