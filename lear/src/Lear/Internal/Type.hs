module Lear.Internal.Type where

import Control.Arrow
import Control.Category
import qualified Data.Bifunctor as Bif
import Data.Monoid.Action
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Lear.Internal.Diff
import Prelude hiding ((.), id)

-- |
-- There's a nice symmetry here, with params showing up once in positive
-- and once in negative position, once with Diff
--
-- Equivalent to UpdateT of Coupdate over diffs. Or, approximately, State of
-- Store.
newtype Lear p a b = Lear {runLear :: p -> a -> (b, Diff b -> (Diff a, Diff p))}
  deriving (Generic)

evalLear :: Lear p a b -> p -> a -> b
evalLear l p a = fst $ runLear l p a

backLear :: (FloatVector a, FloatVector p) => Lear p a b -> p -> a -> b -> (a, p)
backLear l p a b =
  let (b', df) = runLear l p a
      action = df $ subtrAct b b'
   in act action (a, p)

learnOne :: (FloatVector a, FloatVector p) => Lear p a b -> p -> a -> b -> (p, a)
learnOne l p a b = swap $ backLear l p a b

instance Category (Lear p) where
  id = Lear $ \_ a -> (a, (,mempty))
  Lear g . Lear f = Lear $ \p a ->
    let (b, db) = f p a
        (c, dc) = g p b
     in (c, db `compDiff` dc)

instance Arrow (Lear p) where
  arr = error "Never going to happen"
  f &&& g = let Lear h = f *** g in Lear $ \p -> fmap (Bif.first undupDiff) <$$> h p . dup
    where
      dup a = (a, a)
  Lear f *** Lear g = Lear $ \p (b, b') ->
    let (c, dc) = f p b
        (c', dc') = g p b'
     in ( (c, c'),
          \dcc ->
            let (dl, dr) = splitDiff dcc
                (dbl, dpl) = dc dl
                (dbr, dpr) = dc' dr
             in (joinDiff (dbl, dbr), dpl <> dpr)
        )

instance (Action da sa, Action db sb) => Action (da, db) (sa, sb) where
  act (da, db) (sa, sb) = (act da sa, act db sb)

class (Monoid d) => SubtrAction d s where
  subtrAct :: s -> s -> d

instance SubtrAction (Diff p) p where
  subtrAct a b = Diff [(a, 1), (b, -1)]

instance Fractional b => Num (Lear p a b) where
  a + b = plusL . (a &&& b)
  a - b = minusL . (a &&& b)
  a * b = timesL . (a &&& b)

-- * Basic Lears

param :: Lear p a p
param = Lear $ \p _ -> (p, (mempty,))

input :: Lear p a a
input = id

-- * Helpers

plusL :: Num a => Lear p (a, a) a
plusL = Lear $ \p (a, a') -> (a + a', \da -> (dup <$> scaleDiff 0.5 da, mempty))
  where
    dup a = (a, a)

minusL :: Num a => Lear p (a, a) a
minusL = Lear $ \p (a, a') -> (a - a', \da -> (dup <$> scaleDiff 0.5 da, mempty))
  where
    dup a = (a, - a)

timesL :: Fractional a => Lear p (a, a) a
timesL = Lear $ \p (a, a') -> (a * a', \da -> (dup <$> da, mempty))
  where
    dup a = (a, a)

infixl 4 <$$>

(<$$>) :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
(<$$>) = fmap . fmap
