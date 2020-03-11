module Main where

import Data.Bifunctor
import Data.Functor.Foldable
import Lear
import Numeric.Backprop
import Test.Hspec

-- Annoyingly, can't import lear-backprop (see
-- https://github.com/commercialhaskell/stack/issues/4786). So we just copy it
backpropToLear ::
  (Backprop p, Backprop a, Backprop b, Num p, Num a, Num b) =>
  (forall s. (Reifies s W) => BVar s p -> BVar s a -> BVar s b) ->
  Lear p a b
backpropToLear f = Lear f'
  where
    f' p a =
      let (b, g) = backpropWith2 f p a
       in ( b,
            \b' ->
              let (p', a') = g (b' - b) in (const (p - p'), a - a')
          )

main :: IO ()
main =
  hspec $ do
    runLearSpec
    learnOneSpec

runLearSpec :: Spec
runLearSpec =
  describe "runLear"
    $ it "should be the original function"
    $ runLear linear 5 3 `shouldBe` 15

learnOneSpec :: Spec
learnOneSpec =
  describe "learnOne" $ do
    it "returns the input if correct" $
      learnOne linear 5 3 15 `shouldBe` (5, 3)
    it "returns the two fixes if incorrect" $
      learnOne linear 5 3 1 `shouldBe` (5, 3)

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

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear Int Int Int
linear = backpropToLear $ \p x -> p * x

na :: a
na = error "not relevant"
