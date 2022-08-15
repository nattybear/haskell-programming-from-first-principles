module List where

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs)
    = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  Cons f fs <*> xs
    = append (fmap f xs)
             (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys
  = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> List b)
        -> List a
        -> List b
flatMap f as = concat' $ fmap f as
