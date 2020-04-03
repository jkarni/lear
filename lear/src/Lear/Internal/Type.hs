module Lear.Internal.Type where

import Control.Applicative
import qualified Control.Category as C
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor.Rep
import Data.Group
import qualified Data.Map as Map
import Data.Monoid
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import Debug.Trace
import GHC.Generics (Generic)

newtype Lear p a b
  = Lear
      { getLear :: p -> a -> (b, b -> (Endo p, Endo a))
      }
  deriving (Generic)

(&&&) :: Lear p a b0 -> Lear p a b1 -> Lear p a (b0, b1)
Lear x &&& Lear y = Lear $ \p a ->
  let (b0, linx) = x p a
      (b1, liny) = y p a
   in ((b0, b1), \(b0', b1') -> linx b0' <> liny b1')

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

(<->) :: Group a => a -> a -> a
a <-> b = a <> invert b

instance (Fractional b) => Num (Lear p a b) where
  fromInteger x = Lear $ \p a -> (fromInteger x, const mempty)
  negate x = x - 2 * x
  a + b = coerce $ conc C.. (coerce $ a &&& b :: Lear p a (Sum b, Sum b))
  a * b = coerce $ conc C.. (coerce $ a &&& b :: Lear p a (Product b, Product b))

instance C.Category (Lear p) where
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
