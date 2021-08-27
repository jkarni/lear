{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showCcc #-}


module Lear.Internal.Combinators where

import Data.Bifunctor
import Data.Proxy
import Data.Dynamic
import Data.Monoid
import Data.Traversable
import Data.VectorSpace
  ( VectorSpace (..),
    lerp,
  )
import GHC.Show (showList__)
import Lear.Internal.Type
import System.Random
import Type.Reflection ((:~~:) (HRefl), TypeRep, eqTypeRep, typeRep)

data Param c where
  Param :: c a => TypeRep a -> a -> Param c

instance (c `Implies` Show) => Show (Param c) where
  show (Param t a) = show a
  showsPrec i (Param t a) = showsPrec i a
  showList = error "TODO"

-- showList = showList__ (showsPrec 0)


toParam :: (c a, Typeable a) => a -> Param c
toParam = Param typeRep

fromParam :: forall a c. Typeable a => Param c -> Maybe a
fromParam (Param t p)
  | Just HRefl <- t `eqTypeRep` t' = Just p
  | otherwise = error $ "I have type: " ++ show t ++ " but you want: " ++ show (typeRep :: TypeRep a)
  where
    t' = typeRep :: TypeRep a


newParamFor :: Lear P a b -> IO (Param P)
newParamFor (Lear l) = do
  let p = fst (snd (l undefined undefined) undefined)
  p' <- randomIO
  pure . toParam $ (p' `asType` p)

asType :: a -> a -> a
asType a b = a

-- | Not quite batch, since there are more ps. Need to combine them.
batch :: (forall p. c p => c (f p), Applicative f) => Lear c a b -> Lear c (f a) (f b)
batch (Lear f) = Lear $ \fp fa ->
  let (fb, ffb) = pull $ f <$> fp <*> fa
      pull x = (fst <$> x, snd <$> x)
   in (fb, \fbs -> pull $ ffb <*> fbs)

learnOne ::
  (c `Implies` Typeable) =>
  Lear c a b ->
  a ->
  b ->
  Param c ->
  Maybe (Param c, a)
learnOne (Lear f) a b p = case fromParam p of
  Just p' -> Just $ first toParam $ snd (f p' a) b
  Nothing -> Nothing

runLear :: (c `Implies` Typeable) => Lear c a b -> Param c -> a -> Maybe b
runLear (Lear f) p a = case fromParam p of
  Just p' -> Just $ fst $ f p' a
  Nothing -> Nothing

runLear' :: (c `Implies` Random, c `Implies` Typeable) =>
    Lear c a b -> a -> IO (Param c, b)
runLear' (Lear f) a = do
  p <- randomIO
  return (toParam p, fst $ f p a)


{-# INLINE runLear'' #-}
runLear'' :: Lear P a () -> a -> IO (Param P, a)
runLear'' (Lear f) a = do
  p <- randomIO
  let (b', back') = f p a
      (p', a') = back' ()
  return (toParam p', a')

learnMany :: Lear P a () -> [a] -> IO [(Param P, a)]
learnMany (Lear f) as = do
  p <- randomIO
  let go xp xa =
        let (b', back') = f xp xa
            (p', a') = back' ()
         in (p', (p', a'))
  return $ fmap (first toParam) $ snd $ mapAccumL go p as

learnMany' :: Lear P a Double -> [a] -> IO [(Param P, a, Double)]
learnMany' (Lear f) as = do
  p <- randomIO
  let go xp inp =
        let (loss, back') = f xp inp
            (p', a') = back' (- loss)
            newP = xp - (0.01 * p')
         in (newP, (newP, a', loss))
  return $ fmap (\(a, b, c) -> (toParam a, b, c)) $ snd $ mapAccumL go p as

learMany :: Net a b -> [(a, b)] -> IO [(Param P, Loss)]
learMany Net { model = Lear f , loss = diff  } inputs = do
  p <- randomIO
  let -- go :: p -> (a, b) -> (p, (p, Loss))
      go xp (xa, xb) =
        let (b', back') = f xp xa
            (p', a') = back' xb
            newP = xp - (0.1 * p')
         in (newP, (newP, diff xb b'))
  return $ fmap (first toParam) $ snd $ mapAccumL go p inputs

back :: (c `Implies` Random, c `Implies` Typeable) =>
    Lear c a b -> a -> b -> IO (Param c, a)
back (Lear f) a b = do
  p <- randomIO
  let (b', back') = f p a
      (p', a') = back' b
  return (toParam p', a')

{-
backprop :: Lear a b -> p -> a -> (b, b -> (p, a))
backprop (Lear f) = f


learnOne :: Lear a b -> p -> a -> b -> (p, a)
learnOne l p a = snd (backprop l p a)
(<?) :: Lear a b -> b -> (p, a) -> p
(<?) l b (p, a) = fst $ learnOne l p a b

(?>) :: (p, a) -> Lear a b -> b
(p, a) ?> l = runLear l p a

-- | Make a learner never learn.
--
-- Instead, it always sends as update whatever it received, and always asks for
-- input whatever it received.
stultify :: Lear a b -> Lear a b
stultify (Lear f) = Lear $ \p a -> (fst $ f p a, const $ (p, a))
{-
-- | Multiply learning rate by a scalar.
atRate :: VectorSpace (Lear a b) => Lear a b -> Scalar p -> Lear a b
atRate l = lerp (stultify l) l
-- | Add an error function. This means setting the learning rate to the linear
-- interpolation (weighted by resulting scalar) of "total learning" and "no
-- learning".
--
-- Error functions are like learning rates in that they cannot be inspected,
-- but can be composed and "undone". That is, if f x y * g x y == 1, then
--
-- > withError f . withError g == id
withError ::
  (Scalar p ~ Scalar a, VectorSpace p, VectorSpace a) =>
  -- | actual -> expected -> err
  (a -> a -> Scalar a) ->
  Lear a a
withError errFn = Lear $ \_ a ->
  (a, \a' -> let s = errFn a a' in ((s *^), lerp a a' s))
-}
-}
