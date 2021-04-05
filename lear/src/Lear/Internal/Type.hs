{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Lear.Internal.Type where

import Control.Applicative
import Control.Category
import Data.Bifunctor (bimap)
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor.Rep
import Data.Group
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import Debug.Trace
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import Generics.OneLiner
import System.Random
import Prelude hiding ((.), id)

data Lear (c :: * -> Constraint) a b where
  Lear :: (c p) => (p -> a -> (b, b -> (p, a))) -> Lear c a b

class (f x, g x) => And f g x

instance (f x, g x) => And f g x

type Lear' = Lear (Typeable `And` Random)

firstL :: (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Lear c a b -> Lear c (a, d) (b, d)
firstL l = l <***> id

secondL :: (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Lear c a b -> Lear c (d, a) (d, b)
secondL l = id <***> l

infixr 3 <***>

(<***>) :: (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Lear c a0 b0 -> Lear c a1 b1 -> Lear c (a0, a1) (b0, b1)
Lear x <***> Lear y = Lear $ \(pl, pr) (a0, a1) ->
  let (b0, linx) = x pl a0
      (b1, liny) = y pr a1
   in ( (b0, b1),
        \(b0', b1') ->
          let (ep0, ea0) = linx b0'
              (ep1, ea1) = liny b1'
           in ((ep0, ep1), (ea0, ea1))
      )

infixr 3 <&&&>

(<&&&>) :: (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Lear c a b0 -> Lear c a b1 -> Lear c a (b0, b1)
Lear x <&&&> Lear y = Lear $ \(pl, pr) a ->
  let (b0, linx) = x pl a
      (b1, liny) = y pr a
   in ( (b0, b1),
        \(b0', b1') ->
          let (ep0, ea0) = linx b0'
              (ep1, ea1) = liny b1'
           in ((ep0, ep1), ea1)
      )

fstL :: (c ()) => Lear c (a, b) a
fstL = Lear $ \() (a, b) -> (a, \a' -> ((), (a', b)))

sndL :: (c ()) => Lear c (a, b) b
sndL = Lear $ \() (a, b) -> (b, \b' -> ((), (a, b')))

{-
param :: Lear a p
param = Lear $ \p a -> (p, \p' -> (p', a))

input :: Lear a a
input = Lear $ \p a -> (a, \a' -> (p, a'))

flipL :: Lear a b -> Lear a p b
flipL (Lear f) = Lear $ \a p ->
  let (b, linx) = f p a
   in (b, swap <$> linx)

onParam :: Lear a p p -> Lear a a
onParam l = sndL . (flipL l <&&&> id)
-}

-- foldG :: (Group a, Functor f, Foldable f) => Lear (f a) a
-- foldG = Lear $ \p fa ->
--   let res = fold fa
--    in (res, \a -> (mempty, Endo $ \fa' -> (\a' -> a' <> (a <-> res)) <$> fa'))

fanOut :: (c ()) => (Representable f) => Lear c a (f a)
fanOut = Lear $ \() a -> (tabulate (const a), const ((), a))

{-
manyOf :: (Applicative f, Traversable f, Foldable f) => Lear a b -> Lear (f p) (f a) (f b)
manyOf (Lear f) = Lear $ \fp fa ->
  let fs = f <$> fp <*> fa
      fbs = fst <$> fs
      frs = snd <$> fs
   in (fbs, _)
-}

(<->) :: Group a => a -> a -> a
a <-> b = a <> invert b

instance (Num a, c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Num (Lear c a a) where
  fromInteger x = Lear $ \() a -> (fromInteger x, const ((), a))
  Lear a + Lear b = Lear $ \(pl, pr) x ->
    let (av, ad) = a pl x
        (bv, bd) = b pr x
     in ( av + bv,
          \c ->
            let (ap, ac) = (ad c)
                (bp, bc) = (bd c)
             in ((ap, bp), ac + bc)
        )

  -- in (av + bv, \b -> binaryOp @Num (+) (ad b) (bd b))
  Lear a * Lear b = Lear $ \(pl, pr) x ->
    let (av, ad) = a pl x
        (bv, bd) = b pr x
     in ( av * bv,
          \c ->
            let (ap, ac) = (ad c)
                (bp, bc) = (bd c)
             in ((ap, bp), av * bc + bv * ac)
        )

instance (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Category (Lear c) where
  id = Lear $ \() a -> (a, const ((), a))

  Lear g . Lear f = Lear $ \(pl, pr) a ->
    let (b, f') = f pl a
        (c, g') = g pr b
     in ( c,
          \c' ->
            let (pg, b') = g' c'
                (pf, a') = f' b'
             in ((pf, pg), a')
        )
