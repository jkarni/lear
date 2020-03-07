module Lear.AsVector where

import GHC.TypeNats

data Vec n a where
  EmptyVec :: Vec 0 a
  (:::) :: a -> Vec n a -> Vec (n + 1) a

infixr 3 :::

newtype Measure a = Measure {getMeasure :: (a -> Double) -> Double}

-- | A class for embedding and projecting from R^n.
--
-- For most datatypes, constructors can be considered the bases of the vector
-- space that they define. If however some constructors are colinear,
class FormsBasis a n where
  proj :: Vec n Double -> Measure a
  embed :: a -> Vec n Double
{-
instance AsVector Int 1 where
  embed d =
    let low = floor d
        diff = d - fromIntegral low
     in (low, diff) ::: (ceiling d, 1 - diff) ::: EmptyVec
  classify = fromIntegral
-}
