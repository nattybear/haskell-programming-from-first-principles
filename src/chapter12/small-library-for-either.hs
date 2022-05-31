lefts' :: [Either a b] -> [a]
lefts' = foldr (\x acc -> case x of
                            Right _ -> acc
                            Left e  -> e:acc) []

rights' :: [Either a b] -> [b]
rights' = foldr (\x acc -> case x of
                             Left _   -> acc
                             Right x' -> x':acc) []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (\z (xs, ys) -> case z of
                                            Left x  -> (x:xs, ys)
                                            Right y -> (xs, y:ys)) ([], [])

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left e)  = f e
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
