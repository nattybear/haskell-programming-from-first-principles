module Exercise where

newtype Reader r a =
  Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f =>
            (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f
