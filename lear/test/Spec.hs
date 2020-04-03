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
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding ((.))

data Linear
  = Linear
      { weight :: Float,
        bias :: Float
      }
  deriving (Eq, Show, Generic)

main :: IO ()
main =
  hspec $ do
    runLearSpec
    learnOneSpec

-- atRateSpec

runLearSpec :: Spec
runLearSpec =
  describe "runLear" $ do
    it "should be the original function" $ runLear linear 5 3 `shouldBe` 15
    it "work for lens composition" $ do
      let l = Linear {weight = 5, bias = 7}
      runLear linear' l 3 `shouldBe` 22

learnOneSpec :: Spec
learnOneSpec =
  describe "learnOne" $ do
    it "returns the input if correct" $ hedgehog $ do
      x <- H.forAll $ H.float (HR.constant (-100) 100)
      y <- H.forAll $ H.float (HR.constant (-100) 100)
      learnOne linear x y (x * y) ~=~ (x, y)
    it "returns the two fixes if incorrect" $ hedgehog $ do
      x <- H.forAll $ H.float (HR.constant (-100) 100)
      y <- H.forAll $ H.float (HR.constant (-100) 100)
      z <- H.forAll $ H.float (HR.constant (-100) 100)
      let (x', y') = learnOne linear x y z
      x' * y ~=~ z
      x * y' ~=~ z
    it "gets closer if it can't learn in one" $ hedgehog $ do
      w <- H.forAll $ H.float (HR.constant (-100) 100)
      b <- H.forAll $ H.float (HR.constant (-100) 100)
      x <- H.forAll $ H.float (HR.constant (-100) 100)
      y <- H.forAll $ H.float (HR.constant (-100) 100)
      let l = Linear {weight = w, bias = b}
      let l' = (l, x) & linear' <? y
      (l', x) ?> linear' ~=~ y

-- * Helpers

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear Float Float Float
linear = param * input

linear' :: Lear Linear Float Float
linear' = (look @"weight") . param * input + (look @"bias") . param

-- ** Approximate Equality

infix 4 ~=~

(~=~) :: (HasCallStack, MonadTest m, Show a, ApproximateEq a) => a -> a -> m ()
x ~=~ y = withFrozenCallStack $ diff x (~~) y

infix 4 ~~

class ApproximateEq a where
  (~~) :: a -> a -> Bool
  default (~~) :: (Ord a, Fractional a) => a -> a -> Bool
  a ~~ b = abs (a - b) < 0.1

instance ApproximateEq Float

instance (ApproximateEq a, ApproximateEq b) => ApproximateEq (a, b) where
  (a0, a1) ~~ (b0, b1) = a0 ~~ b0 && a1 ~~ b1
