module Constant where

newtype Constant a b = Constant { getConstant :: a }
                     deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant v) = Constant v

-- |
-- >>> const 2 (getConstant (Constant 3))
-- 2
-- >>> fmap (const 2) (Constant 3)
-- Constant {getConstant = 3}
-- >>> gc = getConstant
-- >>> c  = Constant 3
-- >>> gc $ fmap (const 2) c
-- 3
-- >>> gc $ fmap (const "blah") c
-- 3

-- | Testing identity
-- >>> getConstant (id (Constant 3))
-- 3
-- >>> getConstant (fmap id (Constant 3))
-- 3

-- | Composition of the const function
-- >>> ((const 3) . (const 5)) 10
-- 3
-- >>> ((const 5) . (const 3)) 10
-- 5

-- | Composition
-- >>> fc  = fmap (const 3)
-- >>> fc' = fmap (const 5)
-- >>> separate = fc . fc'
-- >>> c  = const 3
-- >>> c' = const 5
-- >>> fused = fmap (c . c')
-- >>> cw = Constant "WOOHOO"
-- >>> getConstant $ separate $ cw
-- "WOOHOO"
-- >>> cdr = Constant "Dogs rule"
-- >>> getConstant $ fused $ cdr
-- "Dogs rule"
