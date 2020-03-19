module Lear.Internal.Combinators where

import Data.Bifunctor
import Data.VectorSpace
  ( VectorSpace (..),
    lerp,
  )
import Lear.Internal.Type

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
withError errFn = Lear $ \_ a ->
  (a, \a' -> let s = errFn a a' in ((s *^), lerp a a' s))
