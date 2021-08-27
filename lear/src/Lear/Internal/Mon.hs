{-# LANGUAGE ApplicativeDo #-}
module Lear.Internal.Mon where

import ConCat.Rebox ()
import Data.Bifunctor
import Control.Applicative
import ConCat.Dual
import ConCat.GAD (GD(..))
import ConCat.AltCat (toCcc')
import ConCat.AdditiveFun

data Mon a where
  Mon :: {
    param :: p,
    runMon :: p -> (b, b -> p)
  } -> Mon b

instance Functor Mon where
  fmap f (Mon par m) = Mon par $ \p ->
     let (a, ap) = m p
         (b, ba) = der f a
      in (b, ap . ba )

instance Applicative Mon where
  pure x = Mon () $ \() -> (x, const ())
  liftA2 f (Mon parA a) (Mon parB b) = Mon (parA, parB) $ \(pa, pb) ->
     let (aa, aap) = a pa
         (ba, bap) = b pb
         (c, cab) = der (uncurry f) (aa, ba)
      in (c, bimap aap bap . cab)

{-# INLINE newParam #-}
newParam :: a -> Mon a
newParam a = Mon a (, id)

{-# INLINE der #-}
der :: forall a b . (a -> b) -> (a -> (b, b -> a))
der fn =
  let fnWithoutP = unD (toCcc' fn :: GD (Dual (-+>)) a b)
   in \a -> let (b, Dual (AddFun bFn)) = fnWithoutP a
                in (b, bFn)

{-
sgd :: [Mon a] -> Mon [a]
sgd ms =
  where
   [ fn p | Mon p fn <- ms ]
-}

minimize :: Num a => Mon a -> (a, Mon a)
minimize (Mon p fn) = let (a, app) = fn p in (a, Mon (app 0) fn)
-- Test

linear :: Double -> Mon Double
linear x = do
  a <- newParam 0.1
  b <- newParam 0.5
  pure $ a * x + b


