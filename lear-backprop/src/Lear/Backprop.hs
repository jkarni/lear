module Lear.Backprop where

import           Lear
import           Numeric.Backprop

backpropToLear
  :: (Backprop p, Backprop a, Backprop b)
  => (forall s. (Reifies s W) => BVar s p -> BVar s a -> BVar s b)
  -> Learn p a b
backpropToLear f = Learn
  { impl = uncurry $ evalBP2 f
  -- Note that in req and upd we're returning the 'wanted' p or a, rather than
  -- some midpoint between old and new p/a.
  , req = \(p,a,b) -> snd $ gradBP2 f p a
  , upd = \(p,a,b) -> fst $ gradBP2 f p a
  }
