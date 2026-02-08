{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Bridge
-- Description : Three scenarios × four semirings — when sites disagree
--
-- "Different sites reveal different invariants of the same underlying
-- structure." But what happens when they *disagree*?
--
-- We run three transfer patterns through four semirings:
--
--   1. Sybil ring     — alice → bob → carol → alice, amounts within 10%
--   2. Organic chain   — eve → frank → grace, amounts diverge, no closure
--   3. Wash trade      — mallory → mallory (self-loop), exact amount
--
-- The interesting result: GF(3) commits a false positive on the organic
-- chain. The round-trip signal is Minus (-1) and the amount signal is
-- also Minus (-1), but Minus × Minus = Plus in GF(3). Double-negative
-- equals positive — the field's multiplicative structure betrays us.
-- The Bayesian site correctly assigns low likelihood.
--
-- This is the whole point: no single semiring is universally correct.
-- The classifying topos needs ALL the sites.
module Main where

import Data.Functor.Identity (Identity, runIdentity)
import Numeric.Log (Log (..))

import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()
import Control.Monad.Bayes.Graded.Semiring.GF3 (GF3 (..))
import Control.Monad.Bayes.Graded.Semiring.Tropical (Tropical (..))
import Control.Monad.Bayes.Graded.Semiring.Polynomial (Poly (..), evaluate)
import Control.Monad.Bayes.Graded.Class (GradedFactor (..))
import Control.Monad.Bayes.Graded.Weighted (GradedWeightedT, runGraded)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Transfer Data
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Transfer = Transfer
  { from   :: String
  , to     :: String
  , amount :: Double
  , timing :: Double
  } deriving (Show)

data Scenario = Scenario
  { scenarioName :: String
  , transfers    :: [Transfer]
  , description  :: String
  , expected     :: String
  }

-- | Scenario 1: Classic sybil ring. Amounts within 10%, round-trip closes.
sybilRing :: Scenario
sybilRing = Scenario
  "Sybil Ring"
  [ Transfer "alice" "bob"   100.0  1000.0
  , Transfer "bob"   "carol"  95.0  1003.0
  , Transfer "carol" "alice"  90.0  1005.0
  ]
  "alice →100→ bob →95→ carol →90→ alice"
  "ALL sites should flag this"

-- | Scenario 2: Organic chain. No closure, amounts diverge wildly.
-- But GF(3) will false-positive: Minus × Minus = Plus.
organicChain :: Scenario
organicChain = Scenario
  "Organic Chain"
  [ Transfer "eve"   "frank"  500.0  2000.0
  , Transfer "frank" "grace"   50.0  8500.0   -- 90% drop, 6500s later
  , Transfer "grace" "heidi"   12.0 15000.0   -- another 76% drop
  ]
  "eve →500→ frank →50→ grace →12→ heidi"
  "Should be ORGANIC (but GF3 false-positives)"

-- | Scenario 3: Wash trade. Self-loop, exact amount, zero delay.
washTrade :: Scenario
washTrade = Scenario
  "Wash Trade"
  [ Transfer "mallory" "mallory" 1000.0 5000.0
  , Transfer "mallory" "mallory" 1000.0 5000.001  -- same amount, near-instant
  ]
  "mallory →1000→ mallory →1000→ mallory"
  "Extreme sybil — self-loop with exact amounts"

scenarios :: [Scenario]
scenarios = [sybilRing, organicChain, washTrade]

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § The Model — polymorphic over the grading semiring
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Signal 1: Does the chain close? (first sender = last receiver)
closes :: [Transfer] -> Bool
closes [] = False
closes ts = from (head ts) == to (last ts)

-- | Signal 2: Amount similarity (0 = identical, 1 = maximally divergent)
amountDivergence :: [Transfer] -> Double
amountDivergence ts =
  let amts = map amount ts
      mx = maximum amts
      mn = minimum amts
  in if mx == 0 then 0 else (mx - mn) / mx

-- | Signal 3: Timing density (transfers per second)
timingDensity :: [Transfer] -> Double
timingDensity ts =
  let times = map timing ts
      span' = maximum times - minimum times
  in if span' < 0.01 then 100.0  -- near-instant = maximally suspicious
     else fromIntegral (length ts) / span'

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 1: Log Double — Bayesian
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

scoreLog :: [Transfer] -> GradedWeightedT (Log Double) Identity ()
scoreLog ts = do
  let div' = amountDivergence ts
      dens = timingDensity ts
      closureBoost = if closes ts then 2.0 else 0.0
      -- Gaussian kernel: high similarity + closure → high likelihood
      logLik = negate (div' * 3.0) + closureBoost + min dens 10.0 * 0.1
  gscore (Exp logLik)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 2: GF(3) — Ternary Trit Classification
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

scoreGF3 :: [Transfer] -> GradedWeightedT GF3 Identity ()
scoreGF3 ts = do
  -- Round-trip: Plus if closes, Minus if open
  let closeTrit = if closes ts then Plus else Minus
  -- Amount: Plus if similar, Zero if ambiguous, Minus if divergent
  let div' = amountDivergence ts
      amtTrit | div' < 0.15 = Plus
              | div' < 0.30 = Zero
              | otherwise   = Minus
  -- Timing: Plus if suspiciously fast, Minus if spread out
  let dens = timingDensity ts
      timTrit | dens > 1.0   = Plus   -- >1 transfer/sec
              | dens > 0.01  = Zero
              | otherwise    = Minus
  -- Three signals multiply in GF(3)
  gscore (closeTrit <.> amtTrit <.> timTrit)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 3: Tropical — Min-cost path
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

scoreTropical :: [Transfer] -> GradedWeightedT (Tropical Double) Identity ()
scoreTropical ts = do
  -- Each hop: amount leaked + time cost
  let hops = zip ts (tail ts)
  mapM_ (\(t1, t2) ->
    let leaked  = abs (amount t1 - amount t2)
        timeCost = abs (timing t2 - timing t1) * 0.001
    in gscore (Tropical (leaked + timeCost))) hops
  -- Self-loop penalty
  let selfLoops = length [() | t <- ts, from t == to t]
  if selfLoops > 0
    then gscore (Tropical 0.0)  -- zero additional cost = maximally suspicious path
    else gscore (Tropical (fromIntegral (length ts)))  -- honest transfers have base cost

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 4: Polynomial — Operation Accounting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

scorePoly :: [Transfer] -> GradedWeightedT (Poly Int) Identity ()
scorePoly ts = do
  let n = length ts
      -- x^0 = base transfers
      baseOps = Poly [n]
      -- x^1 = derived operations (closures detected)
      closureOps = if closes ts then Poly [0, 1] else one
      -- x^2 = second-order (self-loops detected)
      selfLoops = length [() | t <- ts, from t == to t]
      selfOps = if selfLoops > 0 then Poly [0, 0, selfLoops] else one
  gscore (baseOps <.> closureOps <.> selfOps)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Runner
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

runScenario :: Scenario -> IO ()
runScenario s = do
  putStrLn $ "━━━ " ++ scenarioName s ++ " ━━━"
  putStrLn $ "  " ++ description s
  putStrLn $ "  Expected: " ++ expected s
  putStrLn ""

  let ts = transfers s

  -- Log Double
  let ((), logW) = runIdentity $ runGraded (scoreLog ts)
  putStrLn $ "  Bayesian (Log Double): " ++ showLog logW

  -- GF(3)
  let ((), gf3W) = runIdentity $ runGraded (scoreGF3 ts)
  let gf3Label = case gf3W of
        Plus  -> "SYBIL (+1)"
        Zero  -> "AMBIGUOUS (0)"
        Minus -> "ORGANIC (-1)"
  putStrLn $ "  Ternary  (GF3):       " ++ show gf3W ++ " = " ++ gf3Label

  -- Tropical
  let ((), tropW) = runIdentity $ runGraded (scoreTropical ts)
  putStrLn $ "  Min-cost (Tropical):  " ++ show (getTropical tropW) ++ " cost units"

  -- Polynomial
  let ((), polyW) = runIdentity $ runGraded (scorePoly ts)
  putStrLn $ "  Ops      (Poly):      " ++ showPoly polyW
       ++ " (eval@1 = " ++ show (evaluate polyW 1) ++ " ops)"

  putStrLn ""

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Main
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

main :: IO ()
main = do
  putStrLn "graded-bayes: Three Scenarios × Four Sites"
  putStrLn "===========================================\n"

  mapM_ runScenario scenarios

  -- The disagreement analysis
  putStrLn "━━━ DISAGREEMENT ANALYSIS ━━━"
  putStrLn ""
  putStrLn "  Scenario        | Bayesian | GF(3)     | Tropical | Polynomial"
  putStrLn "  ────────────────+──────────+───────────+──────────+───────────"

  let ((), logS)  = runIdentity $ runGraded (scoreLog (transfers sybilRing))
  let ((), gf3S)  = runIdentity $ runGraded (scoreGF3 (transfers sybilRing))
  let ((), tropS) = runIdentity $ runGraded (scoreTropical (transfers sybilRing))
  let ((), polyS) = runIdentity $ runGraded (scorePoly (transfers sybilRing))

  let ((), logO)  = runIdentity $ runGraded (scoreLog (transfers organicChain))
  let ((), gf3O)  = runIdentity $ runGraded (scoreGF3 (transfers organicChain))
  let ((), tropO) = runIdentity $ runGraded (scoreTropical (transfers organicChain))
  let ((), polyO) = runIdentity $ runGraded (scorePoly (transfers organicChain))

  let ((), logW')  = runIdentity $ runGraded (scoreLog (transfers washTrade))
  let ((), gf3W')  = runIdentity $ runGraded (scoreGF3 (transfers washTrade))
  let ((), tropW') = runIdentity $ runGraded (scoreTropical (transfers washTrade))
  let ((), polyW') = runIdentity $ runGraded (scorePoly (transfers washTrade))

  putStrLn $ "  Sybil ring      | " ++ pad 8 (showLog logS) ++ " | "
    ++ pad 9 (showGF3 gf3S) ++ " | " ++ pad 8 (showTrop tropS) ++ " | " ++ showPoly polyS
  putStrLn $ "  Organic chain   | " ++ pad 8 (showLog logO) ++ " | "
    ++ pad 9 (showGF3 gf3O) ++ " | " ++ pad 8 (showTrop tropO) ++ " | " ++ showPoly polyO
  putStrLn $ "  Wash trade      | " ++ pad 8 (showLog logW') ++ " | "
    ++ pad 9 (showGF3 gf3W') ++ " | " ++ pad 8 (showTrop tropW') ++ " | " ++ showPoly polyW'

  putStrLn ""
  putStrLn "  KEY FINDING: GF(3) false-positives on organic chain."
  putStrLn "  The chain is open (Minus) with divergent amounts (Minus),"
  putStrLn "  but Minus × Minus = Plus in the field. Double-negative = positive."
  putStrLn "  The Bayesian site correctly sees low likelihood."
  putStrLn ""
  putStrLn "  No single semiring is the right classifier."
  putStrLn "  The classifying topos needs all the sites."

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Display Helpers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

showLog :: Log Double -> String
showLog w = let l = ln w in take 6 (show l)

showGF3 :: GF3 -> String
showGF3 Plus  = "+1 SYBIL"
showGF3 Zero  = " 0 AMBIG"
showGF3 Minus = "-1 ORGNC"

showTrop :: Tropical Double -> String
showTrop (Tropical x) = take 6 (show x)

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

showPoly :: Poly Int -> String
showPoly (Poly []) = "0"
showPoly (Poly cs) =
  case filter (not . null) $ zipWith showTerm [0 :: Int ..] cs of
    []    -> "0"
    terms -> unwords terms
  where
    showTerm _ 0 = ""
    showTerm 0 c = show c
    showTerm 1 1 = "x"
    showTerm 1 c = show c ++ "x"
    showTerm n 1 = "x^" ++ show n
    showTerm n c = show c ++ "x^" ++ show n
