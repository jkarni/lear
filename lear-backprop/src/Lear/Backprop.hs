module Lear.Backprop where

import Data.Bifunctor
import Lear
import Numeric.Backprop

backpropToLear ::
  (Backprop p, Fractional p) =>
  (forall s. (Reifies s W) => BVar s p -> BVar s p -> BVar s p) ->
  Lear p p p
backpropToLear f = Lear f'
  where
    f' p a =
      let (b, g) = backpropWith2 f p a
       in ( b,
            \b' ->
              let (p', a') = g 1 in (const (p - (b - b') / p'), a - (b - b') / a')
          )
