import Data.Monoid
import Test.QuickCheck

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

-- |
-- >>> onlySum = Only (Sum 1)
-- >>> onlySum `mappend` onlySum
-- Only (Sum {getSum = 2})

-- |
-- >>> onlyFour = Only (Product 4)
-- >>> onlyTwo  = Only (Product 2)
-- >>> onlyFour `mappend` onlyTwo
-- Only (Product {getProduct = 8})

-- |
-- >>> Only (Sum 1) `mappend` Nada
-- Only (Sum {getSum = 1})

-- |
-- >>> Only [1] `mappend` Nada
-- Only [1]

-- |
-- >>> Nada `mappend` Only (Sum 1)
-- Only (Sum {getSum = 1})

instance Semigroup a => Semigroup (Optional a) where
  Nada   <> x      = x
  x      <> Nada   = x
  Only x <> Only y = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
