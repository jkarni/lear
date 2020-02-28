module Lear where

import qualified Control.Category        as C
import           Data.AdditiveGroup
import           Data.Functor.Adjunction
import           Data.Functor.Foldable
import           Data.VectorSpace
import           GHC.Generics            (Generic)

data Learn p a b = Learn
    { impl :: (p,a) -> b
    , upd  :: (p, a, b) -> p
    , req  :: (p, a, b) -> a
    } deriving (Generic)

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

instance (AdditiveGroup p, AdditiveGroup a, AdditiveGroup b)
  => AdditiveGroup (Learn p a b)

instance (VectorSpace p, VectorSpace a, VectorSpace b
  , Scalar b ~ Scalar p, Scalar a ~ Scalar b)
  => VectorSpace (Learn p a b) where
  type Scalar (Learn p a b) = Scalar p
  s *^ l = Learn
    { impl = \(p,a) -> s *^ impl l (p,a)
    , upd = \(p,a,b) -> s *^ upd l (p,a,b)
    , req = \(p,a,b) -> s *^ req l (p,a,b)
    }

-- | Make a learner never learn.
--
-- Instead, it always sends as update whatever it received, and always asks for
-- input whatever it received.
stultify :: Learn p a b -> Learn p a b
stultify l = l { upd = \(p,a,b) -> p, req = \(p,a,b) -> a }

forwards :: Learn p a b -> p -> a -> b
forwards = curry . impl

-- | Multiply learning rate by a scalar.
atRate :: VectorSpace (Learn p a b) => Learn p a b -> Scalar p -> Learn p a b
atRate l r = lerp l (stultify l) r

-- | Lift a learner via adjunctions.
adjunct :: (Eq (l ()), Adjunction l r) => Learn p a b -> Learn (r p) (l a) b
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
