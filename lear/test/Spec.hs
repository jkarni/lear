module Main where

import Control.Category
import Control.Lens.TH
import Data.Bifunctor
import Data.Coerce
import Data.Functor.Foldable
import Data.Group
import Data.Monoid (Sum (..))
import Data.VectorSpace ()
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

main :: IO ()
main =
  hspec $ do
    runLearSpec
    learnOneSpec

-- atRateSpec

runLearSpec :: Spec
runLearSpec =
  describe "runLear" $ do
    it "should be the original function" $ do
      let l = Linear {weight = 5, bias = 7}
      runLear linear l 3 `shouldBe` 15
    it "work for lens composition" $ do
      let l = Linear {weight = 5, bias = 7}
      runLear linear' l 3 `shouldBe` 22

learnOneSpec :: Spec
learnOneSpec =
  describe "learnOne" $ do
    context "linear" $ testLear linear
    context "linear'" $ testLear linear'
  where
    -- context "linearSub" $ testLear linearSub

    testLear lear = do
      it "returns the input if correct" $ hedgehog $ do
        (w, b, x) <- (,,) <$> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        learnOne lear l x (runLear lear l x) ~=~ (l, x)
      it "returns the fixed param if incorrect" $ hedgehog $ do
        (w, b, x, y) <- (,,,) <$> floatGen <*> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        let (l', _) = learnOne lear l x y
        runLear lear l' x ~=~ y
      it "returns the fixed input if incorrect" $ hedgehog $ do
        (w, b, x, y) <- (,,,) <$> floatGen <*> floatGen <*> floatGen <*> floatGen
        let l = Linear {weight = w, bias = b}
        let (_, x') = learnOne lear l x y
        runLear lear l x' ~=~ y

-- * Helpers

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear Linear Float Float
linear = #weight . param * input

linear' :: Lear Linear Float Float
linear' = #weight . param * input + #bias . param

linearSub :: Lear Linear Float Float
linearSub = #weight . param - input

{-
foldLinear :: Lear Linear [Float] (Float)
foldLinear = linear . foldG
-}

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
