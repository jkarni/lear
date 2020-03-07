module Main where

import Data.Functor.Foldable
import Lear
import Test.Hspec

main :: IO ()
main =
  hspec
    cataLSpec

{-
adjunctSpec :: Spec
adjunctSpec = describe "adjunct" $ do
-}

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

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear Int Int Int
linear = liftNum $ \p x -> p * x

na :: a
na = error "not relevant"
