{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showCcc #-}

module Main
  ( main,
  )
where

import Control.Category
import Control.Lens.TH
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor
import Data.Coerce
import ConCat.Rebox ()
import GHC.Generics
import ConCat.AltCat ()
import Data.Functor.Foldable
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
import System.Random
import Test.Hspec
import Test.Hspec.Hedgehog
import Control.Monad
import Prelude hiding ((.), id)

main :: IO ()
main = do
  let inputs = (\x -> (x, x * 3)) <$> [0,0.01..1]
  foo
  xs <- learnMany' t'' inputs
  forM_ (zip inputs xs) $ \((a, b), (p, pred, loss)) ->
    putStrLn $
       "p: " ++ show p ++
       "\na: " ++ show a ++
       "\nb: " ++ show b ++
       "\npred: " ++ show pred ++
       "\nloss: " ++ show loss ++
       "\n\n"
{-
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
      l <- newParamFor linear
      runLear linear l 3 `shouldBe` Just 15
    it "work for lens composition" $ do
      l <- newParamFor linear'
      runLear linear' l 3 `shouldBe` Just 22

learnOneSpec :: Spec
learnOneSpec =
  describe "learnOne" $ do
    context "linear" $ testLear linear
    context "linear'" $ testLear linear'
  where
    -- context "linearSub" $ testLear linearSub

    testLear :: Lear NeedC Float Float -> Spec
    testLear lear = do
      it "returns the input if correct" $ hedgehog $ do
        (b, x) <- (,) <$> floatGen <*> floatGen
        l <- liftIO $ newParamFor lear
        case runLear lear l x of
          Nothing -> liftIO $ expectationFailure "Should be Just"
          Just y -> learnOne lear x y l ~=~ Just (l, x)
      it "returns the fixed param if incorrect" $ hedgehog $ do
        (b, x, y) <- (,,) <$> floatGen <*> floatGen <*> floatGen
        l <- liftIO $ newParamFor lear
        let Just (l', _) = learnOne lear x y l
        runLear lear l' x ~=~ Just y
      it "returns the fixed input if incorrect" $ hedgehog $ do
        (b, x, y) <- (,,) <$> floatGen <*> floatGen <*> floatGen
        l <- liftIO $ newParamFor lear
        let Just (_, x') = learnOne lear x y l
        liftIO . print $ ("orig", runLear lear l x)
        liftIO . print $ ("new", runLear lear l x')
        liftIO . print $ ("goal", Just y)
        runLear lear l x' ~=~ Just y

-- * Helpers

type NeedC = Typeable `And` Show `And` Random `And` ApproximateEq

class (a ~ b) => Is a b

instance (a ~ b) => Is a b

-- | A linear function passing through the origin without noise.
--
-- This is solvable from one datapoint, which makes testing easier.
linear :: Lear NeedC Float Float
linear = param * id

linear' :: Lear NeedC Float Float
linear' = param * id + param

linearSub :: Lear NeedC Float Float
linearSub = param - id

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

instance ApproximateEq () where
  _ ~~ _ = True

instance (c `Implies` ApproximateEq) => ApproximateEq (Param c) where
  Param t1 v1 ~~ Param t2 v2 =
    case t1 `eqTypeRep` t2 of
      Nothing -> False
      Just HRefl -> v1 ~~ v2

instance ApproximateEq a => ApproximateEq (Maybe a) where
  Just x ~~ Just y = x ~~ y
  Nothing ~~ Nothing = True
  _ ~~ _ = False

instance Random Linear where
  random g =
    let (a, g') = random g
        (b, g'') = random g'
     in (Linear a b, g'')
-}
