module Lear.Spans where

import GHC.TypeNats

data Vec n a where
  EmptyVec :: Vec 0 a
  (:::) :: a -> Vec n a -> Vec (n + 1) a

infixr 3 :::

newtype Measure a = Measure {getMeasure :: (a -> Double) -> Double}

-- | A class for embedding and projecting from R^n.
--
-- For most datatypes, constructors can be considered the bases of the vector
-- space that they define. If however constructors are not linearly
-- independent, then there is a more effective representation.
class Spans a n where
  proj :: Vec n Double -> Measure a
  embed :: a -> Vec n Double

-- | Makes a continuous function given a function from a FormsBasis type.
inputAsVec :: Spans a n => (a -> Double) -> Vec n Double -> Double
inputAsVec f v = getMeasure (proj v) f

-- This one is a little more dubious, since it's not really continuous.
-- Really what we want is the signature:
---
--  asVec :: (FormsBasis a n, FormsBasis b m) => (a -> b) -> Vec n Double -> Vec m Double
--
-- Since then we can account for discontinuities.
outputAsVec :: Spans a n => (Double -> a) -> Double -> Vec n Double
outputAsVec f d = embed $ f d
