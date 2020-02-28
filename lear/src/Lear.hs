module Lear where

import qualified Control.Category        as C
import           Data.Functor.Adjunction
import           Data.Functor.Foldable

data Learn p a b = Learn
    { impl :: (p,a) -> b
    , upd  :: (p, a, b) -> p
    , req  :: (p, a, b) -> a
    }

forwards :: Learn p a b -> p -> a -> b
forwards = curry . impl

-- TODO:
--
-- A nicer way to do this is to make `Learn p a b` a vector space over F.
-- Then we define either:
--   dumb :: (p -> a -> b) -> Learn p a b
-- or
--   stultify :: Learn p a b -> Learn p a b
--
-- Which for upd and req just return the original version. Then we can say
-- something like:
--
-- rate :: r -> Learn p a b -> Learn p a b
-- rate r l = (r .* stultify l) - ((1 - r) .* l)
--
addRate
  :: (Fractional p, Fractional a)
  => Double -> Learn p a b -> Learn p a b
addRate r l = l
  { upd = \(p,a,b) -> p - (fromRational (toRational r) * upd l (p,a,b))
  , req = \(p,a,b) -> a - (fromRational (toRational r) * req l (p,a,b))
  }


instance C.Category (Learn p) where
  id = Learn
    { impl = (\(_,x) -> x)
    , upd = (\(p,_,_) -> p)
    , req = (\(_,a,_) -> a)
    }
  f . g = Learn
    { impl = i
    , req = r
    , upd = u
    }
    where
      i (p,a) = impl f (p, impl g (p, a))
      r (p,a,c) =
        let b = impl g (p, a)
            req0 = req f (p,b,c)
        in req g (p,a,req0)
      u (p,a,c) =
        let b = impl g (p,a)
        in upd g (upd f (p,b,c), a, req f (p,b,c))

adjunct :: (Eq (l ()), Adjunction l r)
    => Learn p a b -> Learn (r p) (l a) b
adjunct lear = Learn
    { impl = uncurry $ zapWithAdjunction (curry $ impl lear)
    , upd = \(rp,la,b) ->
        let zapped = zapWithAdjunction (\p a -> upd lear (p,a,b)) rp la
        in tabulateAdjunction (\lun -> if lun == (const () <$> la)
                                       then zapped
                                       else indexAdjunction rp lun)
    , req = \(rp,la,b) ->
        let zapped = zapWithAdjunction (\p a -> req lear (p,a,b)) rp la
        in const zapped <$> la
    }


cataL :: (Recursive t) => Learn p (Base t a) a -> Learn p t a
cataL l = Learn
  { impl = i
  , upd = \(p,t,a) -> zygo (curry (impl l) p) (\b -> upd l (p,fst <$> b, a)) t
  , req = error "not impl"
  }
  where
    i (p,t) = cata (curry (impl l) p) t

{-

anaLearn :: (Corecursive t) => Learn p a (Base t a) -> Learn p a t
anaLearn = _

liftLens :: Lens a b -> Learn p a b
liftLens = _
-}
