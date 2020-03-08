{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lear.Spans where

import Control.Monad.Cont
import GHC.TypeNats

data Vec n a where
  EmptyVec :: Vec 0 a
  (:::) :: a -> Vec n a -> Vec (n + 1) a

infixr 3 :::

instance {-# OVERLAPPING #-} Num a => Num (Vec 1 a) where
  fromInteger x = fromInteger x ::: EmptyVec
  (a ::: _) + (b ::: _) = a + b ::: EmptyVec
  (a ::: _) * (b ::: _) = a * b ::: EmptyVec
  (a ::: _) - (b ::: _) = a - b ::: EmptyVec
  abs (a ::: _) = abs a ::: EmptyVec
  signum (a ::: _) = signum a ::: EmptyVec

{- TODO: Do this properly
instance {-# OVERLAPPABLE #-} (Num a, Num (Vec (n - 1) a)) => Num (Vec n a) where
  fromInteger x = fromInteger x ::: (fromInteger x :: Vec (n - 1) a)
  (a ::: as) + (b ::: bs) = a + b ::: (as + bs :: Vec (n - 1) a)
  (a ::: as) * (b ::: bs) = a * b ::: (as * bs :: Vec (n - 1) a)
  (a ::: as) - (b ::: bs) = a - b ::: (as - bs :: Vec (n - 1) a)
  abs (a ::: as) = abs a ::: (abs as :: Vec (n - 1) a)
  signum (a ::: as) = signum a ::: (signum as :: Vec (n - 1) a)
-}

-- | A class for embedding and projecting from R^n.
--
-- For most datatypes, constructors can be considered the bases of the vector
-- space that they define. If however constructors are not linearly
-- independent, then there is a more effective representation.
--
-- The class *almost* defines an Iso. But the class constraint of the `Cont`
-- allows keeping multiple elements of the type, and doing some interpolation
-- on the result.
class Spans a n where
  proj :: forall b. RealFrac b => Vec n b -> Cont b a
  embed :: forall b. RealFrac b => a -> Vec n b

instance Spans Int 1 where
  embed x = fromIntegral x ::: EmptyVec
  proj (a ::: _) = cont $ \f ->
    let low = floor a
        high = ceiling a
        diff = a - fromIntegral low
     in -- Picking linear interpolation here. This might not be the best choice,
        -- since it introduces discontinuities in the higher-order derivatives.
        (1 - diff) * f low + diff * f high

-- | Makes a continuous function given a function from a Span type.
inputAsVec :: (RealFrac b, Spans a n) => (a -> b) -> Vec n b -> b
inputAsVec f v = runCont (proj v) f
{-
-- This one is a little more dubious, since it's not really continuous.
-- Really what we want is the signature:
---
--  asVec :: (FormsBasis a n, FormsBasis b m) => (a -> b) -> Vec n Double -> Vec m Double
--
-- Since then we can account for discontinuities.
outputAsVec :: Spans a n => (Double -> a) -> Double -> Vec n Double
outputAsVec f d = embed $ f d

asVec :: (Spans a n, Spans b m) => (a -> b) -> Vec n Double -> Vec m Double
asVec f v = runCont (proj v) _
-}
