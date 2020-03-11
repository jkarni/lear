module Lear.AD where

import Lear
import Numeric.AD

adToLear ::
  (Num a, Num a, Num a) =>
  (forall s. (Reifies s W) => BVar s p -> BVar s a -> BVar s b) ->
  Lear p a b
adToLear f = Learn
