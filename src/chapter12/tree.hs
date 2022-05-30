data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f v = case f v of
               Nothing      -> Leaf
               Just (x,y,z) -> Node l y r
                 where l = unfold f x
                       r = unfold f z
