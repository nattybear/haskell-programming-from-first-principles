-- Let's write code

--   1. The following function returns the tens digit of an integral
--      argument:

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

--      a) First, rewrite it using divMod.

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_    , d) = xLast `divMod` 10

--      b) Dose the divMod version have the same type as the original
--         version?

--         Yes.

--      c) Next, let's change it so that we're getting the hundreds digit
--         instead. You could start it like this (though that may not be
--         the only possibility):

hunsD x = d2
  where d  = x `div` 100
        d2 = d `mod` 10

--   2. Implement the following function of the type a -> a -> Bool ->
--      a once using a case expression and once with a guard:

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    False -> x
    True  -> y

foldBool' x y b
  | b == False = x
  | b == True  = y

--      The result is semantically similar to if-then-else expressions
--      but syntactically quite diffenrent. Here is the pattern matching
--      version to get you started:

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

--   3. Fill in the definition. Note that the first argument to our function
--      is also a function that can be applied to values. Your second
--      argument is a tuple, which can be used for pattern matching.

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
