{-# LANGUAGE LambdaCase #-}

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee y _ Nothing  = y
mayybee y f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe y Nothing  = y
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = map (\case Just x -> x) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if any isNothing xs
               then Nothing
               else Just (catMaybes xs)
