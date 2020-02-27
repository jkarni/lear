module Main where

import           Data.Functor.Foldable
import           Lear
import           Test.Hspec

main :: IO ()
main = hspec $ do
  cataLSpec

cataLSpec :: Spec
cataLSpec = describe "cataL" $ do

  it "combines impl correctly" $ do
    let i x = case x of
            Nil      -> 0
            Cons a b -> a + b
        l = Learn { impl = \(p,a) -> p * i a, upd = na, req = na }
    forwards (cataL l) 0 [2,3,1] `shouldBe` 0
    forwards (cataL l) 1 [2,3,1] `shouldBe` 6

na :: a
na = error "not relevant"
