module OpticLaws (opticTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Numeric.Log (Log (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()
import Control.Monad.Bayes.Graded.Semiring.GF3 (GF3 (..))
import Control.Monad.Bayes.Graded.Optic

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Simple graded lenses for testing
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A graded lens that doubles the input, grade = Plus.
doubleLens :: GradedOptic GF3 Int Int Int Int
doubleLens = fromLens
  (\n -> (n * 2, Plus))
  (\_ m -> (m, Plus))

-- | A graded lens that adds 1, grade = Minus.
incLens :: GradedOptic GF3 Int Int Int Int
incLens = fromLens
  (\n -> (n + 1, Minus))
  (\_ m -> (m, Minus))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Identity Laws
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | identity |>>>| f  has same grades as f
prop_identityLeft :: Int -> Int -> Bool
prop_identityLeft s b =
  let (a1, _t1, gf1, gb1) = runOptic (identity |>>>| doubleLens) s b
      (a2, _t2, gf2, gb2) = runOptic doubleLens s b
  in a1 == a2 && gf1 == gf2 && gb1 == gb2

-- | f |>>>| identity  has same grades as f
prop_identityRight :: Int -> Int -> Bool
prop_identityRight s b =
  let (a1, _t1, gf1, gb1) = runOptic (doubleLens |>>>| identity) s b
      (a2, _t2, gf2, gb2) = runOptic doubleLens s b
  in a1 == a2 && gf1 == gf2 && gb1 == gb2

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Grade Composition
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Forward grade of composed optic = product of forward grades.
prop_forwardGradeComposes :: Int -> Int -> Bool
prop_forwardGradeComposes s b =
  let (_, _, gf_composed, _) = runOptic (doubleLens |>>>| incLens) s b
      (_, gf_outer) = forwardOnly doubleLens s
      (_, gf_inner) = forwardOnly incLens (fst (forwardOnly doubleLens s))
  in gf_composed == gf_outer <.> gf_inner

-- | Total grade = forward × backward.
prop_totalIsProduct :: Int -> Int -> Bool
prop_totalIsProduct s b =
  let (_, _, gf, gb) = runOptic doubleLens s b
  in totalGrade doubleLens s b == gf <.> gb

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Identity grades are one
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

prop_identityGradesAreOne :: Int -> Int -> Bool
prop_identityGradesAreOne s b =
  let (_, _, gf, gb) = runOptic (identity @GF3) s b
  in gf == one && gb == one

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Para laws
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Simple Para: linear model y = w * x, graded by Log Double.
linearPara :: Para (Log Double) Double Double
linearPara = Para
  (2.0 :: Double)                            -- initial weight
  (\w x -> (w * x, Exp (negate 0.1)))        -- forward: y = wx, small cost
  (\w x _fb -> (w + 0.01 * x, Exp (negate 0.5)))  -- backward: gradient step

-- | runPara uses the forward pass.
prop_paraForward :: Bool
prop_paraForward =
  let (y, g) = runPara linearPara 3.0
  in y == 6.0 && g == Exp (negate 0.1)

-- | stepPara returns both grades and updated Para.
prop_paraStep :: Bool
prop_paraStep =
  let (y, gf, gb, _) = stepPara linearPara 3.0 0.0
  in y == 6.0 && gf == Exp (negate 0.1) && gb == Exp (negate 0.5)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Cyber laws
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Simple cybernetic thermostat: state = target temp.
thermostat :: Cyber GF3 Double Double
thermostat = Cyber
  (20.0 :: Double)                      -- target temp
  (\target obs ->
    let diff = obs - target
        act = negate diff * 0.1          -- heating/cooling
        grade = if abs diff > 5 then Plus else Minus
    in (act, grade))
  (\target _obs feedback ->
    let target' = target + feedback * 0.01
        grade = if abs feedback > 1 then Plus else Minus
    in (target', grade))

prop_cyberStep :: Bool
prop_cyberStep =
  let (act, gf, gb, _) = stepCyber thermostat 30.0 0.5
  in act == negate (30.0 - 20.0) * 0.1  -- -1.0
     && gf == Plus                       -- |diff| = 10 > 5
     && gb == Minus                      -- |feedback| = 0.5 < 1

prop_cyberGrade :: Bool
prop_cyberGrade =
  let g = gradeCyber thermostat 30.0 0.5
  in g == Plus <.> Minus  -- Plus × Minus = Minus

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Instances
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

instance Arbitrary GF3 where
  arbitrary = elements [Minus, Zero, Plus]

instance Arbitrary (Log Double) where
  arbitrary = Exp . negate . abs <$> (arbitrary :: Gen Double)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Test Tree
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

opticTests :: TestTree
opticTests = testGroup "Graded Optic Laws"
  [ testGroup "Identity"
    [ testProperty "identity |>>>| f ≡ f (grades)" prop_identityLeft
    , testProperty "f |>>>| identity ≡ f (grades)" prop_identityRight
    , testProperty "identity grades are one" prop_identityGradesAreOne
    ]
  , testGroup "Composition"
    [ testProperty "forward grade composes multiplicatively" prop_forwardGradeComposes
    , testProperty "total grade = forward × backward" prop_totalIsProduct
    ]
  , testGroup "Para"
    [ testProperty "runPara uses forward pass" $ once prop_paraForward
    , testProperty "stepPara returns both grades" $ once prop_paraStep
    ]
  , testGroup "Cyber"
    [ testProperty "stepCyber runs play/coplay" $ once prop_cyberStep
    , testProperty "gradeCyber = play × coplay" $ once prop_cyberGrade
    ]
  ]
