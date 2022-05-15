data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left x right) = Node (mapTree f left)
                                     (f x)
                                     (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf)
                   2
                   (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup OK!"
          else error "test failed!"

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf)
                2
                (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = concat [ [x]
                                      , preorder left
                                      , preorder right
                                      ]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = concat [ preorder left
                                     , [x]
                                     , preorder right
                                     ]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = concat [ preorder left
                                       , preorder right
                                       , [x]
                                       ]

testPreorder :: IO ()
testPreorder = if preorder testTree == [2, 1, 3]
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inorder testTree == [1, 2, 3]
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postorder testTree == [1, 3, 2]
                then putStrLn "Postorder fine!"
                else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
