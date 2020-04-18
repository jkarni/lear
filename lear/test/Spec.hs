module Main where

import Control.Category
import Control.Lens.TH
import Data.Bifunctor
import Data.Functor.Foldable
import Data.VectorSpace
import GHC.Generics (Generic)
import GHC.Stack
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as HR
import Lear
import Numeric.Backprop
import Numeric.OneLiner
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding ((.))

data Linear
  = Linear
      { weight :: Float,
        bias :: Float
      }
  deriving stock (Eq, Show, Generic)
  deriving (Num) via (GNum Linear)

instance AdditiveGroup Linear

instance VectorSpace Linear

main :: IO ()
main =
  hspec $ do
    basicSpec

-- atRateSpec

basicSpec :: Spec
basicSpec =
  describe "For " $ do
    context "multiplication" $ testLear timesLin
    context "multiplication and addition '" $ testLear timesPlusLin
    context "subtraction" $ testLear minusLin
  where
    testLear lear = do
      it "returns the input if correct" $ hedgehog $ do
        (w, b, x) <- (,,) <$> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        learnOne lear l x (evalLear lear l x) ~=~ (l, x)
      it "returns the fixed param if incorrect" $ hedgehog $ do
        (w, b, x, y) <- (,,,) <$> floatGen <*> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        let (l', _) = learnOne lear l x y
        evalLear lear l' x ~=~ y
      it "returns the fixed input if incorrect" $ hedgehog $ do
        (w, b, x, y) <- (,,,) <$> floatGen <*> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        let (_, x') = learnOne lear l x y
        evalLear lear l x' ~=~ y

-- * Helpers

timesLin :: Lear Linear Float Float
timesLin = #weight . param * input

timesPlusLin :: Lear Linear Float Float
timesPlusLin = #weight . param * input + #bias . param

minusLin :: Lear Linear Float Float
minusLin = #weight . param - input

-- * Generators

floatGen :: (HasCallStack, Monad m) => PropertyT m Float
floatGen = H.forAll $ H.float (HR.constant (-100) 100)

-- ** Approximate Equality

infix 4 ~=~

(~=~) :: (HasCallStack, MonadTest m, Show a, ApproximateEq a) => a -> a -> m ()
x ~=~ y = withFrozenCallStack $ diff x (~~) y

infix 4 ~~

class ApproximateEq a where
  (~~) :: a -> a -> Bool
  default (~~) :: (Ord a, Fractional a) => a -> a -> Bool
  a ~~ b = 0.1 > (abs (a - b) / (a + b))

instance ApproximateEq Float

instance (ApproximateEq a, ApproximateEq b) => ApproximateEq (a, b) where
  (a0, a1) ~~ (b0, b1) = a0 ~~ b0 && a1 ~~ b1

instance ApproximateEq Linear where
  Linear a0 a1 ~~ Linear b0 b1 = a0 ~~ b0 && a1 ~~ b1
