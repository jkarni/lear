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
import Data.Tuple (swap)
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import Debug.Trace
import GHC.Generics (Generic)
import Prelude hiding ((.), id)

newtype Lear p a b
  = Lear
      { getLear :: p -> a -> (b, b -> (Endo p, Endo a))
      }
  deriving (Generic)

firstL :: Lear p a b -> Lear p (a, d) (b, d)
firstL l = l <***> id

secondL :: Lear p a b -> Lear p (d, a) (d, b)
secondL l = id <***> l

infixr 3 <***>

(<***>) :: Lear p a0 b0 -> Lear p a1 b1 -> Lear p (a0, a1) (b0, b1)
Lear x <***> Lear y = Lear $ \p (a0, a1) ->
  let (b0, linx) = x p a0
      (b1, liny) = y p a1
   in ( (b0, b1),
        \(b0', b1') ->
          let (ep0, Endo ea0) = linx b0'
              (ep1, Endo ea1) = liny b1'
           in (ep0 <> ep1, Endo $ bimap ea0 ea1)
      )

infixr 3 <&&&>

(<&&&>) :: Lear p a b0 -> Lear p a b1 -> Lear p a (b0, b1)
Lear x <&&&> Lear y = Lear $ \p a ->
  let (b0, linx) = x p a
      (b1, liny) = y p a
   in ((b0, b1), \(b0', b1') -> linx b0' <> liny b1')

fstL :: Lear p (a, b) a
fstL = Lear $ \p (a, b) -> (a, \a' -> (mempty, Endo $ \(_, b') -> (a', b')))

sndL :: Lear p (a, b) b
sndL = Lear $ \p (a, b) -> (b, \b' -> (mempty, Endo $ \(a', _) -> (a', b')))

param :: Lear p a p
param = Lear $ \p a -> (p, \p' -> (Endo $ const p', mempty))

input :: Lear p a a
input = Lear $ \p a -> (a, \a' -> (mempty, Endo $ const a'))

flipL :: Lear p a b -> Lear a p b
flipL (Lear f) = Lear $ \a p ->
  let (b, linx) = f p a
   in (b, swap <$> linx)

onParam :: Lear a p p -> Lear p a a
onParam l = sndL . (flipL l <&&&> id)

foldG :: (Group a, Functor f, Foldable f) => Lear p (f a) a
foldG = Lear $ \p fa ->
  let res = fold fa
   in (res, \a -> (mempty, Endo $ \fa' -> (\a' -> a' <> (a <-> res)) <$> fa'))

fanOut :: (Representable f) => Lear p a (f a)
fanOut = Lear $ \_ a -> (tabulate (const a), const mempty)

{-
manyOf :: (Applicative f, Traversable f, Foldable f) => Lear p a b -> Lear (f p) (f a) (f b)
manyOf (Lear f) = Lear $ \fp fa ->
  let fs = f <$> fp <*> fa
      fbs = fst <$> fs
      frs = snd <$> fs
   in (fbs, _)
-}

conc :: (Group a) => Lear p (a, a) a
conc =
  Lear $ \p (a0, a1) ->
    let res = a0 <> a1
     in (res, \a -> (mempty, Endo $ \(a0', a1') -> (a0 <> (a <-> res), a1 <> (a <-> res))))

inverse :: (Group p, Group a) => Lear p a a
inverse = Lear $ \p a ->
  ( invert a,
    -- The const here is wrong, since it ignores sideways changes. Which in
    -- turn means endo is going to be wrong...
    \a' -> (mempty, Endo $ const $ invert a')
  )

(<->) :: Group a => a -> a -> a
a <-> b = a <> invert b

instance (Num p, Fractional b) => Num (Lear p a b) where
  fromInteger x = Lear $ \p a -> (fromInteger x, const mempty)
  a + b = coerce $ conc . (coerce $ a <&&&> b :: Lear p a (Sum b, Sum b))
  a * b = coerce $ conc . (coerce $ a <&&&> b :: Lear p a (Product b, Product b))
  a - b = coerce $ conc . (onParam inverse . (coerce $ a <&&&> b :: Lear (Sum p) a (Sum b, Sum b)))

instance Category (Lear p) where
  id = Lear $ \_ a -> (a, const mempty)

  Lear g . Lear f = Lear $ \p a ->
    let (b, f') = f p a
        (c, g') = g p b
     in ( c,
          \c' ->
            let (pg, b') = g' c'
                (pf, a') = f' (appEndo b' b)
             in (pf <> pg, a')
        )
