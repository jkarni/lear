{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Lear.Internal.Spans where

import Control.Applicative
import Control.Monad.Cont
import Data.Distributive
import Data.Functor.Rep
import GHC.Exts

data Nat = Z | S Nat
  deriving (Show, Eq)

class LTE (a :: Nat) (b :: Nat)

newtype NatLTE (n :: Nat) = NatLTE {getNatLTE :: Int}

data Vec n a where
  EmptyVec :: Vec 'Z a
  (:::) :: a -> Vec n a -> Vec ('S n) a

infixr 3 :::

deriving instance Functor (Vec n)

deriving instance Foldable (Vec n)

deriving instance Traversable (Vec n)

instance Applicative (Vec 'Z) where
  pure _ = EmptyVec
  EmptyVec <*> _ = EmptyVec

instance Applicative (Vec n) => Applicative (Vec ('S n)) where
  pure x = x ::: pure x
  (f ::: fs) <*> (x ::: xs) = f x ::: (fs <*> xs)

instance Representable (Vec n) => Distributive (Vec n) where
  distribute = distributeRep

instance
  ( Representable (Vec n),
    Rep (Vec n) ~ NatLTE n
  ) =>
  Representable (Vec ('S n))
  where
  type Rep (Vec ('S n)) = NatLTE ('S n)
  tabulate f = f (NatLTE 0) ::: tabulate f'
    where
      f' (NatLTE n) = f (NatLTE $ n - 1)
  index (h ::: t) (NatLTE n) = if n == 0 then h else index t (NatLTE $ n - 1)

instance IsList (Vec 'Z a) where
  type Item (Vec 'Z a) = a
  toList _ = []
  fromList _ = EmptyVec

instance (Item (Vec n a) ~ a, IsList (Vec n a)) => IsList (Vec ('S n) a) where
  type Item (Vec ('S n) a) = a
  toList (a ::: as) = a : toList as
  fromList (a : as) = a ::: fromList as
  fromList [] = error "Empty list"

instance
  (Applicative (Vec n), Num (Item (Vec n a)), IsList (Vec n a), Num a) =>
  Num (Vec n a)
  where
  fromInteger x = fromList $ repeat $ fromInteger x
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum

instance
  ( Applicative (Vec n),
    Fractional (Item (Vec n a)),
    IsList (Vec n a),
    Fractional a
  ) =>
  Fractional (Vec n a)
  where
  fromRational x = fromList $ repeat $ fromRational x
  recip = fmap recip

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
  -- Having a type that is embeddable in R^n doesn't tell us everything we
  -- might want to know about the type. If elements are not linearly
  -- independent, it still matters how many elements that type has for
  -- determining the theoretical minimum widths of NN layers [Colah, 2014].
  proj :: forall b. RealFrac b => Vec n b -> Cont b a
  embed :: forall b. RealFrac b => a -> Vec n b

instance Spans Int ('S 'Z) where
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

{-
[Colah, 2014] http://colah.github.io/posts/2014-03-NN-Manifolds-Topology/
 -}
