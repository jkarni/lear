module Lear.SimpleDeriv where

import Data.Derivative
import Data.LinearMap
import Lear

{-
liftDeriv :: (p, a) :~> b -> Lear p a b
liftDeriv f = Lear $ \p a ->
  let D b d = f (p, a)

   in (b, _)
-}
