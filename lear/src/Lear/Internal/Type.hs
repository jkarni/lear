{-# LANGUAGE IncoherentInstances #-}

module Lear.Internal.Type where

import Control.Applicative
import qualified Control.Category as C
import Control.Lens
import Data.Profunctor
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import GHC.Generics (Generic)

{-
newtype Lear p a b
  = Lear
      -- We return a p -> p rather than a p so that updates can be composed.
      -- This is more general than using a product, and also nicer to use since
      -- you can keep classy lenses over p.
      -- That said, I'm not sure updates that are not independent make sense
      -- (prob: that don't commute).
      { getLear :: p -> a -> (b, b -> (p -> p, a))
      }
  deriving (Generic)
-}

-- type Lear p a b = Optic' A_Lens NoIx (p, a) (p, b)

data Lear p a b
  = Lear
      {getLear :: forall f. (Functor f) => ((p, b) -> f (p, b)) -> (p, a) -> f (p, a)}

t :: (Num p, Num a) => Lear p a Int -> Lear p a Int -> Lear p a Int
t = (+)

par :: Lear p a p
par = Lear $ lens (\(p, _) -> (p, p)) const

inp :: Lear p a a
inp = Lear $ lens id (\_ x -> x)

run :: Lear p a b -> (p, a) -> b
run (Lear f) x = snd $ x ^. f

back :: Lear p a b -> (p, a) -> b -> (p, a)
back (Lear f) x b = x & f %~ _ -- \(p, _) -> (p, b)

{-
liftOp ::
  (Num a, Num b) =>
  (forall a. Num a => a -> a -> a) ->
  Lear p a b ->
  Lear p a b ->
  Lear p a b
liftOp (?) (Lear x) (Lear y) = Lear $ \p a ->
  let (bx, linx) = x p a
      (by, liny) = y p a
   in ( bx ? by,
        \b' ->
          let (fpx, ax) = linx b'
              (fpy, ay) = liny b'
           in -- Not sure about ax ? ay...
              (fpx . fpy, ax ? ay)
      )
-}

instance (Num p, Num a) => Num (Lear p a b)
-- negate = fmap negate
-- Lear a + Lear b = Lear $ \f g -> liftA2 (++) (a f g) (b f g)
--   where
--     (++) :: (Num x, Num y) => (x, y) -> (x, y) -> (x, y)
--     (a0, b0) ++ (a1, b1) = (a0 + a1, b0 + b1)
-- (*) = liftA2 (*)
-- fromInteger = pure . fromInteger
-- abs = fmap abs
-- signum = fmap signum
{-
instance (Num a, Num b) => Num (Lear p a b) where
  fromInteger x = Lear $ \p a -> (fromInteger x, const (const p, a))
  (+) = liftOp (+)
  (-) = liftOp (-)
  (*) = liftOp (*)
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
