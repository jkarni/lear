module Lear.Backprop where

import Data.Bifunctor
import Lear
import Numeric.Backprop

backpropToLear ::
  (Backprop p, Backprop a, Backprop b) =>
  (forall s. (Reifies s W) => BVar s p -> BVar s a -> BVar s b) ->
  Lear p a b
backpropToLear f = Lear $ tweak $ backpropWith2 f
  where
    -- I kid you not
    tweak = fmap (fmap (second (fmap (first const))))
