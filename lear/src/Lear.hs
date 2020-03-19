{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lear where

import qualified Control.Category as C
import Control.Lens
import Control.Monad
import Data.AdditiveGroup
import Data.Bifunctor
import Data.Functor.Adjunction
import Data.Functor.Foldable
import Data.Functor.Rep (tabulate)
import Data.Generics.Product
import Data.Monoid (Endo (..))
import Data.Tuple (swap)
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
    lerp,
  )
import GHC.Generics (Generic)

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

instance C.Category (Lear p) where
  id = Lear $ \p a -> (a, const (id, a))

  Lear g . Lear f = Lear $ \p a ->
    let (b, f') = f p a
        (c, g') = g p b
     in ( c,
          \c' ->
            let (pg, b') = g' c'
                (pf, a') = f' b'
             in (pf . pg, a')
        )

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
          \b ->
            let (updP, a') = lin b
             in (\p' -> s *^ updP p', s *^ a')
        )

backprop :: Lear p a b -> p -> a -> (b, b -> (p, a))
backprop (Lear f) p a =
  let (b, lin) = f p a
   in (b, first ($ p) . lin)

runLear :: Lear p a b -> p -> a -> b
runLear l p a = fst (backprop l p a)

learnOne :: Lear p a b -> p -> a -> b -> (p, a)
learnOne l p a = snd (backprop l p a)

grad :: Lear p a b -> p -> a -> b -> (p, a)
grad l p a = snd (backprop l p a)

-- | Make a learner never learn.
--
-- Instead, it always sends as update whatever it received, and always asks for
-- input whatever it received.
stultify :: Lear p a b -> Lear p a b
stultify (Lear f) = Lear $ \p a -> (fst $ f p a, const (id, a))

-- | Multiply learning rate by a scalar.
atRate :: VectorSpace (Lear p a b) => Lear p a b -> Scalar p -> Lear p a b
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
  Lear p a a
withError errFn = Lear $ \p a ->
  (a, \a' -> let s = errFn a a' in ((s *^), lerp a a' s))

-- * Adjoints

-- $adjoinNote
-- `adjoin` and `adjoinCheaply` can be seen as two extremes options for how to
-- lift a `Lear` to work on a left-adjoint input. `adjoin` uses the upper bound
-- for the param, whereas `adjoinCheaply` uses the lower bound.

-- | Lift param and input through an adjunction.

-- $adjoinNote

adjoin :: (Eq (l ()), Adjunction l r) => Lear p a b -> Lear (r p) (l a) b
adjoin (Lear f) = Lear $ \rp la ->
  let (b, f') = zapWithAdjunction f rp la
   in ( b,
        \b' ->
          let (updP, a) = f' b'
           in (tweakAdjAtPoint la updP, a <$ la)
      )

-- | Re-use the param for every possible element of a left-adjoint.

-- $adjoinNote

adjoinCheaply :: (Eq (l ()), Adjunction l r) => Lear p a b -> Lear p (l a) b
adjoinCheaply (Lear f) = Lear $ \p la ->
  let (b, f') = zapWithAdjunction f (tabulate $ const p) la
   in ( b,
        \b' ->
          let (updP, a) = f' b'
           in (updP, a <$ la)
      )

-- This Eq constraint is so ugly, and the whole function seems pretty
-- inefficient.
tweakAdjAtPoint :: (Eq (f ()), Adjunction f u) => f a -> (b -> b) -> u b -> u b
tweakAdjAtPoint point f cont = tabulateAdjunction $ \ix ->
  if void point == ix
    then f $ indexAdjunction cont ix
    else indexAdjunction cont ix

con ::
  (Traversable f) =>
  (a -> (p -> p, f a)) ->
  (a -> (p -> p, f a)) ->
  (a -> (p -> p, f (f a)))
con l r a =
  let (p0, b0) = l a
      (p, x) = sequence $ (first Endo <$> r) <$> b0
   in (p0 . appEndo p, x)

data E p a = E (p -> p) !a
  deriving (Functor)

instance Applicative (E p) where
  pure = E id

  E p0 f <*> E p1 a = E (p0 . p1) (f a)

unE :: E p a -> (p -> p, a)
unE (E f a) = (f, a)

type instance Base (E x t) = Base t

instance (Traversable (Base t), Corecursive t) => Corecursive (E x t) where
  embed x = embed <$> sequenceA x

cataL ::
  forall p t a.
  (Applicative (Base t), Corecursive t, Recursive t, Traversable (Base t)) =>
  Lear p (Base t a) a ->
  Lear p t a
cataL (Lear f) = Lear $ \p t ->
  let once :: (a, a -> (p -> p, Base t a))
      once = cata (\x -> f p (fst <$> x)) t
      co :: (a -> (x -> x, Base t a)) -> a -> (x -> x, t)
      co f x = unE $ ana (\(E x a) -> let (p, b) = f a in E p <$> b) (E id x)
   in co <$> once

{-
anaL ::
  forall p t a.
  (Applicative (Base t), Corecursive t, Recursive t, Traversable (Base t)) =>
  Lear p a (Base t a) ->
  Lear p a t
anaL (Lear f) = Lear $ \p t -> _
-}

-- * Lenses

liftLens :: Lens (p, a) (p, a) b b -> Lear p a b
liftLens l = Lear _

liftLens' :: Lens a a b b -> Lear p a b
liftLens' l = Lear _

look :: forall sel a b p. HasAny sel a a b b => Lear p a b
look = liftLens' (the @sel)

param :: Lear p a p
param = Lear $ \p a -> (p, \p' -> (const p', a))

input :: Lear p a a
input = Lear $ \p a -> (a, (const p,))
