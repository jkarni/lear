{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showCcc #-}
-- {-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}
{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
-- {-# OPTIONS_GHC  -fsimpl-tick-factor=20000 #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE Arrows #-}

module Lear.Internal.Type where

import Math.MetricSpace
import Control.Applicative
import Control.Monad
import ConCat.AdditiveFun
import Control.Arrow
import Data.Functor.Contravariant (Op(..))
import Control.Category
import ConCat.Rebox ()
import ConCat.Dual
import ConCat.GAD (GD(..))
import ConCat.AltCat (toCcc')
import Data.Bifunctor (bimap)
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor.Rep
import Data.Group
import qualified Data.Map as Map
import Data.Monoid ()
import Data.Proxy
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import Debug.Trace
import GHC.Exts (Constraint)
import GHC.Generics
import Generics.OneLiner
import System.Random
import Prelude hiding ((.), id)

data Lear (c :: * -> Constraint) a b where
  Lear :: (c p) => { getLear :: (p -> a -> (b, b -> (p, a))) } -> Lear c a b

class (f x, g x) => And f g x

instance (f x, g x) => And f g x

der :: forall a b . (a -> b) -> (a -> (b, b -> a))
der fn =
  let fnWithoutP = unD (toCcc' fn :: GD (Dual (-+>)) a b)
   in \a -> let (b, Dual (AddFun bFn)) = fnWithoutP a
                in (b, bFn)


{-# INLINE deriv #-}
deriv :: forall a b c. c () => (a -> b) -> Lear c a b
deriv fn =
  let fnWithoutP = unD (toCcc' fn :: GD (Dual (-+>)) a b)
   in Lear $ \() a -> let (b, Dual (AddFun bFn)) = fnWithoutP a
                in (b, \b' -> ((), bFn b'))

-- type Lear' = Lear (Typeable `And` Random `And` Show)

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

{-
instance (Num a, c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Num (Lear c a a) where
  fromInteger x = Lear $ \() a -> (fromInteger x, const ((), a))
  Lear a + Lear b = Lear $ \(pl, pr) x ->
    let (av, ad) = a pl x
        (bv, bd) = b pr x
     in ( av + bv,
          \c ->
            let (ap, ac) = ad c
                (bp, bc) = bd c
             in ((ap, bp), ac + bc)
        )

  -- in (av + bv, \b -> binaryOp @Num (+) (ad b) (bd b))
  Lear a * Lear b = Lear $ \(pl, pr) x ->
    let (av, ad) = a pl x
        (bv, bd) = b pr x
     in ( av * bv,
          \c ->
            let (ap, ac) = ad c
                (bp, bc) = bd c
             in ((ap, bp), av * bc + bv * ac)
        )
-}

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

instance (c (), forall pl pr. (c pl, c pr) => c (pl, pr)) => Arrow (Lear c) where
   arr = deriv
   {-# INLINE arr #-}
   first = firstL

class Nil x
instance Nil x

type P = Typeable `And` Random `And` Show `And` Num `And` Fractional

instance Random () where
  randomR _ g = ((), g)
  random g = ((), g)

instance (Random pl, Random pr) => Random (pl, pr) where
  random g =
    let (pl, g') = random g
        (pr, g'') = random g'
     in ((pl, pr), g'')



-- Testing

{-# INLINE loss'' #-}
loss'' :: (b -> b -> l) -> Lear P a b -> Lear P (a, b) l
loss'' d l = proc (a, bExpected) -> do
  bActual <- l -< a
  err <- arr (uncurry d) -< (bActual, bExpected)
  returnA -< err

-- shouldn't be act, but metric?
loss' :: Num b => (b -> b -> l) -> Lear P a b -> Lear P (a, b) l
loss' m (Lear f) = Lear $ \p (a, b) ->
  let (b', lf) = f p a
   in (m b b', \l' ->
        -- TODO: check whether this is b' or b
        let (p', a') = lf b
         in (p', (a', b)) )

class Act dx x | x -> dx, dx -> x where
  act :: dx -> x -> x
  subtrAct :: x -> x -> dx

instance Act Int Int where
  act = (+)
  subtrAct = (-)

instance Act Double Double where
  act = (+)
  subtrAct = (-)

data Net a b = Net
  { model :: Lear P a b
  , loss :: b -> b -> Loss
  }

type Loss = Double


{-# INLINE learningRate #-}
learningRate :: Num l => l -> Lear P l ()
learningRate f = Lear $ \() l -> ((), \() -> ((), f * l))

{-# INLINE gradientDescent #-}
gradientDescent :: Num a => (c `Implies` Num) => Lear c a b -> Lear c a b
gradientDescent (Lear l) = Lear $ \p a ->
  let (b, bfn) = l p a
   in (b, \b' ->
        -- Maybe also on a?
        let (p', a') = bfn b'
         in (p - p', a - a'))

{-# INLINE tuck #-}
tuck :: (c p, forall pl pr. (c pl, c pr) => c (pl, pr)) => Lear c (p, a) b -> Lear c a b
tuck (Lear l) = Lear $ \(p, p') a ->
  let (b, bfn) = l p (p', a)
    in (b, \b' -> let (px, (px', a')) = bfn b
                   in ((px, px'), a'))

t :: Lear P Double Double
t = tuck $ arr $ \(p, x) -> p + (3 * x)

t' :: Lear P (Double, Double) ()
t' = gradientDescent $ learningRate 0.0 . loss'' (\a b -> (a - b) ^ 2) t

t'' :: Lear P (Double, Double) Double
t'' = loss' (\a b -> (a - b) ^ 2) t
-- t'' = loss'' (\_ _ -> 7) t


{-
-}
class (forall x. c1 x => c2 x) => Implies c1 c2

instance (forall x. c1 x => c2 x) => Implies c1 c2

instance Num () where
  _ + _ = ()
  _ * _ = ()
  fromInteger _ = ()
  abs _ = ()
  negate _ = ()

instance (Num a , Num b ) => Num (a, b) where
  (la, lb) + (ra, rb) = (la + ra, lb + rb)
  (la, lb) * (ra, rb) = (la * ra, lb * rb)
  fromInteger x = (fromInteger x, fromInteger x)
  abs (a, b) = (abs a, abs b)
  negate (a, b) = (negate a, negate b)

instance Fractional () where
  fromRational _ = ()
  recip _ = ()

instance (Fractional a, Fractional b) => Fractional (a, b)  where
  fromRational x = (fromRational x, fromRational x)
  recip (a, b) = (recip a, recip b)

foo :: IO ()
foo = do
  let d = der (\x -> 1 + (x * 3))
  forM_ [1..10] $ \x -> do
    let (b, back) = d x
    putStrLn $ "b: " ++ show b ++ " back-1: " ++ show (back 1)
    putStrLn $ "b: " ++ show b ++ " back-1: " ++ show (back 2)
