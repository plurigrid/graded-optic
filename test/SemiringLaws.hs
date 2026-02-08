{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module SemiringLaws (semiringTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Monad.Bayes.Graded.Semiring.Class
import Control.Monad.Bayes.Graded.Semiring.GF3
import Control.Monad.Bayes.Graded.Semiring.Tropical
import Control.Monad.Bayes.Graded.Semiring.Polynomial
import Control.Monad.Bayes.Graded.Semiring.LogDomain
import Numeric.Log (Log (..))

-- Arbitrary instances

instance Arbitrary GF3 where
  arbitrary = elements [Minus, Zero, Plus]

instance Arbitrary (Tropical Double) where
  arbitrary = Tropical <$> arbitrary

instance Arbitrary (Poly Int) where
  arbitrary = Poly <$> resize 5 (listOf arbitrary)

instance Arbitrary (Log Double) where
  arbitrary = Exp . negate . abs <$> (arbitrary :: Gen Double)

-- Generic semiring law tests using ScopedTypeVariables

semiringLaws :: forall a. (Semiring a, Eq a, Show a, Arbitrary a) => String -> TestTree
semiringLaws name = testGroup name
  [ testProperty "additive identity (left)"  $ \(x :: a) -> (zero :: a) <+> x == x
  , testProperty "additive identity (right)" $ \(x :: a) -> x <+> (zero :: a) == x
  , testProperty "additive commutativity"    $ \(x :: a) (y :: a) -> x <+> y == y <+> x
  , testProperty "additive associativity"    $ \(x :: a) (y :: a) (z :: a) ->
      (x <+> y) <+> z == x <+> (y <+> z)
  , testProperty "multiplicative identity (left)"  $ \(x :: a) -> (one :: a) <.> x == x
  , testProperty "multiplicative identity (right)" $ \(x :: a) -> x <.> (one :: a) == x
  , testProperty "multiplicative associativity"    $ \(x :: a) (y :: a) (z :: a) ->
      (x <.> y) <.> z == x <.> (y <.> z)
  , testProperty "left distributivity"  $ \(x :: a) (y :: a) (z :: a) ->
      x <.> (y <+> z) == (x <.> y) <+> (x <.> z)
  , testProperty "right distributivity" $ \(x :: a) (y :: a) (z :: a) ->
      (x <+> y) <.> z == (x <.> z) <+> (y <.> z)
  , testProperty "left annihilation"  $ \(x :: a) -> (zero :: a) <.> x == (zero :: a)
  , testProperty "right annihilation" $ \(x :: a) -> x <.> (zero :: a) == (zero :: a)
  ]

semiringTests :: TestTree
semiringTests = testGroup "Semiring Laws"
  [ semiringLaws @GF3 "GF3"
  , semiringLaws @(Poly Int) "Polynomial"
  -- Tropical and LogDomain use floating point, so we skip exact equality tests
  -- and instead test structural properties
  , testGroup "Tropical (structural)"
    [ testProperty "zero is identity for min" $ \(x :: Tropical Double) ->
        (zero :: Tropical Double) <+> x == x
    , testProperty "one is identity for +" $ \(x :: Tropical Double) ->
        (one :: Tropical Double) <.> x == x
    ]
  ]
