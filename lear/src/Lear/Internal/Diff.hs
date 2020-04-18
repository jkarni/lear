module Lear.Internal.Diff where

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Monoid.Action (Action (..))
import Data.VectorSpace (VectorSpace (..), (^+^), sumV)

-- | Diffs as free vectors
--
-- Another option is something like forall r. Num r => Cont r a
-- But then joining and splitting Diffs is difficult
newtype Diff a = Diff {getDiff :: [(a, Float)]}
  -- Eq instance should maybe normalize?
  deriving stock (Functor, Show, Eq)

compDiff ::
  (Diff b -> (Diff c, Diff p)) ->
  (Diff a -> (Diff b, Diff p)) ->
  Diff a ->
  (Diff c, Diff p)
compDiff g f a =
  let (db, dp) = f a
   in (<> dp) <$> g db

scaleDiff :: Float -> Diff a -> Diff a
scaleDiff s (Diff d) = Diff $ second (s *) <$> d

undupDiff :: Diff (b, b) -> Diff b
undupDiff (Diff vs) = Diff $ (first fst <$> vs) <> (first snd <$> vs)

instance Applicative Diff where
  pure x = Diff [(x, 1)]
  (<*>) = ap

instance Monad Diff where
  Diff as >>= f =
    Diff $
      concat
        [[(b, y * x) | let Diff us = f a, (b, y) <- us] | (a, x) <- as]

instance Semigroup (Diff a) where
  Diff a <> Diff b = Diff $ a <> b

instance Monoid (Diff a) where
  mempty = Diff mempty

type FloatVector p = (Scalar p ~ Float, VectorSpace p)

instance (FloatVector p) => Action (Diff p) p where
  act d p = runDiff d ^+^ p

runDiff :: (FloatVector a) => Diff a -> a
runDiff (Diff as) = sumV [x *^ a | (a, x) <- as]

joinDiff :: (Diff a, Diff b) -> Diff (a, b)
joinDiff (a, b) = (,) <$> a <*> b

splitDiff :: Diff (a, b) -> (Diff a, Diff b)
splitDiff ab = (fst <$> ab, snd <$> ab)
