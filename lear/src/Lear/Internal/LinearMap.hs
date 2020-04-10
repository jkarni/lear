module Lear.Internal.LinearMap where

import Control.Category (Category)
import Data.VectorSpace (AdditiveGroup (..), VectorSpace (..))
import GHC.Exts
import GHC.TypeLits (KnownNat)
import Numeric.LinearAlgebra (scale)
import Numeric.LinearAlgebra.Static

class
  -- I want a map of the cat!
  (forall a b. (Cat map a, Cat map b) => VectorSpace (map a b)) =>
  LinearMap map
  where
  type Cat map :: k -> Constraint
  type Vec map :: k -> *

  -- | A linear map must satisfy:
  --    1) applyLinearMap f (a ^+^ b) == applyLinearMap f a ^+^ applyLinearMap f b
  --    1) applyLinearMap f (s *^ v) == s *^ applyLinearMap f v
  applyLinearMap :: (Cat map dom, Cat map codom) => map dom codom -> Vec map dom -> Vec map codom

  composeMap :: (Cat map m, Cat map k, Cat map n) => map m k -> map k n -> map m n

instance (KnownNat n, KnownNat m) => AdditiveGroup (L n m) where
  zeroV = 0
  (^+^) = (+)
  negateV = negate

instance (KnownNat n, KnownNat m) => VectorSpace (L n m) where
  type Scalar (L n m) = Double
  s *^ m = case create (scale s $ unwrap m) of
    Nothing -> error "impossible?"
    Just m' -> m'

instance LinearMap L where
  type Cat L = KnownNat
  type Vec L = R
  applyLinearMap = app
  composeMap = (Numeric.LinearAlgebra.Static.<>)
