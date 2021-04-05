{-# LANGUAGE UndecidableInstances #-}

module Lear.Internal.Adjoint where

import Control.Monad
import Data.Bifunctor (first)
import Data.Functor.Adjunction
import Data.Functor.Foldable
import Data.Functor.Rep (tabulate)
import Data.Monoid (Endo (..))
import Lear.Internal.Type

-- * Adjoints

-- $adjoinNote
-- `adjoin` and `adjoinCheaply` can be seen as two extremes options for how to
-- lift a `Lear` to work on a left-adjoint input. `adjoin` uses the upper bound
-- for the param, whereas `adjoinCheaply` uses the lower bound.

-- | Lift param and input through an adjunction.

-- $adjoinNote

adjoin :: (Eq (l ()), Adjunction l r) => Lear a b -> Lear (r p) (l a) b
adjoin (Lear f) = Lear $ \rp la ->
  let (b, f') = zapWithAdjunction f rp la
   in ( b,
        \b' ->
          let (updP, updA) = f' b'
           in (tweakAdjAtPoint la updP rp, const updA <$> la)
      )

-- | Re-use the param for every possible element of a left-adjoint.

-- $adjoinNote

adjoinCheaply :: (Eq (l ()), Adjunction l r) => Lear a b -> Lear (l a) b
adjoinCheaply (Lear f) = Lear $ \p la ->
  let (b, f') = zapWithAdjunction f (tabulate $ const p) la
   in ( b,
        \b' ->
          let (updP, updA) = f' b'
           in (updP, fmap (const updA) la)
      )

-- This Eq constraint is so ugly, and the whole function seems pretty
-- inefficient.
tweakAdjAtPoint :: (Eq (f ()), Adjunction f u) => f a -> b -> u b -> u b
tweakAdjAtPoint point f r = tabulateAdjunction $ \ix ->
  if void point == ix
    then f
    else indexAdjunction r ix

con ::
  (Traversable f) =>
  (a -> (p -> p, f a)) ->
  (a -> (p -> p, f a)) ->
  (a -> (p -> p, f (f a)))
con l r a =
  let (p0, b0) = l a
      (p, x) = sequence $ (first Endo <$> r) <$> b0
   in (p0 . appEndo p, x)
{-
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
  Lear (Base t a) a ->
  Lear t a
cataL (Lear f) = Lear $ \p t ->
  let once :: (a, a -> (p -> p, Base t a))
      once = cata (\x -> f p (fst <$> x)) t
      co :: (a -> (x -> x, Base t a)) -> a -> (x -> x, t)
      co f' x = unE $ ana (\(E _ a) -> let (p', b) = f' a in E p' <$> b) (E id x)
   in co <$> once

anaL ::
  forall p t a.
  (Applicative (Base t), Corecursive t, Recursive t, Traversable (Base t)) =>
  Lear a (Base t a) ->
  Lear a t
anaL (Lear f) = Lear $ \p t -> _
-}
