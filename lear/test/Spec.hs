module Main where

import Control.Category
import Data.Bifunctor
import Data.Functor.Foldable
import Data.VectorSpace
import GHC.Stack
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as HR
import Lear
import Numeric.Backprop
import Test.Hspec
import Test.Hspec.Hedgehog
import Prelude hiding ((.))

main :: IO ()
main =
  hspec $ do
    runLearSpec
    learnOneSpec

-- atRateSpec

runLearSpec :: Spec
runLearSpec =
  describe "runLear"
    $ it "should be the original function"
    $ runLear linear 5 3 `shouldBe` 15

learnOneSpec :: Spec
learnOneSpec =
  describe "learnOne" $ do
    it "returns the input if correct" $ hedgehog $ do
      x <- H.forAll $ H.filter (/= 0) $ H.float (HR.constant (-100) 100)
      y <- H.forAll $ H.float (HR.constant (-100) 100)
      learnOne linear x y (x * y) ~=~ (x, y)
    it "returns the two fixes if incorrect" $ hedgehog $ do
      x <- H.forAll $ H.filter (/= 0) $ H.float (HR.constant (-100) 100)
      y <- H.forAll $ H.float (HR.constant (-100) 100)
      z <- H.forAll $ H.float (HR.constant (-100) 100)
      let (x', y') = learnOne linear x y z
      x' * y ~=~ z
      x * y' ~=~ z

{-

atRateSpec :: Spec
atRateSpec =
  describe "atRate" $ do
    prop "doesn't change things if set to one" $ \(x, y, z) ->
      (x /= 0) ==> learnOne linear x y z == learnOne (linear `atRate` 1) x y z
    prop "is the inverse of multiplying the error" $ \(x, y, z) ->
      (x /= 0)
        ==> let res = x * y
             in learnOne linear x y (res + z) ~~ learnOne (linear `atRate` 0.1) x y (res + z / 0.1)

-}

-- * Helpers

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear Float Float Float
linear = param * input

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
{-
adjunctSpec :: Spec
adjunctSpec = describe "adjunct" $ do
-}

{-
cataLSpec :: Spec
cataLSpec = describe "cataL"
  $ it "combines impl correctly"
  $ do
    let i x = case x of
          Nil -> 0
          Cons a b -> a + b
        l = Learn {impl = \(p, a) -> p * i a, upd = na, req = na}
    forwards (cataL l) 0 [2, 3, 1] `shouldBe` 0
    forwards (cataL l) 1 [2, 3, 1] `shouldBe` 6
-}
