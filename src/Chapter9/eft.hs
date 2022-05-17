eftBool :: Bool -> Bool -> [Bool]
eftBool start stop = if start >= stop
                     then [stop]
                     else start : eftBool (succ start) stop

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd start stop = if start >= stop
                    then [stop]
                    else start : eftOrd (succ start) stop

eftInt :: Int -> Int -> [Int]
eftInt start stop = if start >= stop
                    then [stop]
                    else start : eftInt (succ start) stop

eftChar :: Char -> Char -> [Char]
eftChar start stop = if start >= stop
                     then [stop]
                     else start : eftChar (succ start) stop
