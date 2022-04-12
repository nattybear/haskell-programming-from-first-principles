data Identity a =
  Identity a

instance Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
