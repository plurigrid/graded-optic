module Isomorphism (isomorphismTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Numeric.Log (Log (..))
import Control.Monad.Bayes.Graded.Weighted (GradedWeightedT (..), runGraded)
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.GF3
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()  -- bring Semiring (Log a) into scope
import Control.Monad.Bayes.Graded.Class (GradedFactor (..))
import Data.Functor.Identity (Identity, runIdentity)

-- | Test that gscore accumulates multiplicatively.
prop_scoreAccumulates :: Log Double -> Log Double -> Bool
prop_scoreAccumulates w1 w2 =
  let result = runIdentity $ runGraded @(Log Double) (gscore w1 >> gscore w2)
      expected = w1 * w2
  in snd result == expected

-- | Test that gscore one is identity.
prop_scoreOneIdentity :: Bool
prop_scoreOneIdentity =
  let result = runIdentity $ runGraded @(Log Double) (gscore (one :: Log Double))
  in snd result == one

-- | Test GF3 scoring accumulation.
prop_gf3Accumulates :: GF3 -> GF3 -> Bool
prop_gf3Accumulates w1 w2 =
  let result = runIdentity $ runGraded @GF3 (gscore w1 >> gscore w2)
  in snd result == (one <.> w1 <.> w2)

-- | Test that pure doesn't change the grade.
prop_purePreservesGrade :: Int -> Bool
prop_purePreservesGrade x =
  let result = runIdentity $ runGraded @(Log Double) (pure x)
  in snd result == one && fst result == x

instance Arbitrary GF3 where
  arbitrary = elements [Minus, Zero, Plus]

instance Arbitrary (Log Double) where
  arbitrary = Exp . negate . abs <$> (arbitrary :: Gen Double)

isomorphismTests :: TestTree
isomorphismTests = testGroup "Graded Weighted Isomorphism"
  [ testProperty "gscore accumulates multiplicatively (Log Double)" prop_scoreAccumulates
  , testProperty "gscore one = identity (Log Double)" $ once prop_scoreOneIdentity
  , testProperty "gscore accumulates (GF3)" prop_gf3Accumulates
  , testProperty "pure preserves grade" prop_purePreservesGrade
  ]
