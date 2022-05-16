isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf []         []     = True
isSubseqOf _          []     = False
isSubseqOf []         _      = True
isSubseqOf xs'@(x:xs) (y:ys) = if x == y
                               then isSubseqOf xs  ys
                               else isSubseqOf xs' ys
