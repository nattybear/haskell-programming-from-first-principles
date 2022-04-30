mySql = [x^2 | x <- [1..5]]

myCube = [y^3 | y <- [1..5]]

tuples = [ (x,y)
         | x <- mySql
         , y <- myCube
         , x < 50
         , y < 50
         ]
