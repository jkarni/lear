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

{-
newtype FreeVec a = FreeVec {getFreeVec :: Map.Map a Float}

instance

instance AdditiveGroup (FreeVec a) where
  zeroV :: FreeVec mempty
-}

newtype Lear p a b
  = Lear
      -- We return a p -> p rather than a p so that updates can be composed.
      -- This is more general than using a product, and also nicer to use since
      -- you can keep classy lenses over p.
      -- That said, I'm not sure updates that are not independent make sense
      -- (prob: that don't commute).
      { getLear :: p -> a -> (b, b -> (Endo p, Endo a))
      }
  deriving (Generic)

{-
liftOp ::
  (Num a, Num p, Num b, Show a, Show p, Show b) =>
  (forall a. Num a => a -> a -> a) ->
  Lear p a b ->
  Lear p a b ->
  Lear p a b
liftOp (?) (Lear x) (Lear y) = Lear $ \p a ->
  let (bx, linx) = x (trace ("p=" ++ show p) p) (trace ("a=" ++ show a) a)
      (by, liny) = y p (trace ("a=" ++ show a) a)
   in ( (trace ("bx=" ++ show bx) bx) ? (trace ("by=" ++ show by) by),
        \b' ->
          let (fpx, ax) = linx b'
              (fpy, ay) = liny (trace ("b'=" ++ show b') b')
           in -- Not sure about ax ? ay...
              (fpx . fpy, trace ("ax=" ++ show ax) ax - trace ("ay=" ++ show ay) ay)
      )
-}

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

-- x + y = plusL C.. (x &&& y)
-- (+) = (^+^)

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
{-
instance
  (AdditiveGroup p, AdditiveGroup a, AdditiveGroup b) =>
  AdditiveGroup (Lear p a b)

instance
  ( VectorSpace p,
    VectorSpace a,
    VectorSpace b,
    Scalar p ~ Scalar a,
    Scalar p ~ Scalar b
  ) =>
  VectorSpace (Lear p a b)
  where
  type Scalar (Lear p a b) = Scalar p

  s *^ Lear f = Lear $ \p a ->
    let (b, lin) = f p a
     in ( s *^ b,
          \b' ->
            let (updP, a') = lin b'
             in (\p' -> s *^ updP p', s *^ a')
        )
-}
