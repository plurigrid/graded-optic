{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Cybernetic
-- Description : Four sites, two directions, one topos
--
-- Extends the Bridge example to bidirectional graded optics.
-- The same sybil analysis pipeline — composed from two stages —
-- run through four semirings, each revealing different invariants
-- from BOTH the forward (observation) and backward (feedback) channels.
--
-- This demonstrates the novel combination:
-- graded effects + profunctor optics + categorical cybernetics.
module Main where

import Numeric.Log (Log (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()
import Control.Monad.Bayes.Graded.Semiring.GF3 (GF3 (..), toInt)
import Control.Monad.Bayes.Graded.Semiring.Tropical (Tropical (..))
import Control.Monad.Bayes.Graded.Semiring.Polynomial (Poly (..), evaluate)
import Control.Monad.Bayes.Graded.Optic

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § The Observation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Transfers = Transfers
  { amounts :: [Double]
  , timings :: [Double]
  , closes  :: Bool
  } deriving Show

sample :: Transfers
sample = Transfers [100, 95, 90] [1000, 1003, 1005] True

amountSim :: Transfers -> Double
amountSim t = let mx = maximum (amounts t)
                  mn = minimum (amounts t)
              in 1.0 - (mx - mn) / mx

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Generic Pipeline Builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Two-stage sybil analysis, polymorphic over the grading semiring:
--
--   Stage 1 (outer): Amount analysis → similarity score [0,1]
--   Stage 2 (inner): Timing analysis → verdict (Bool)
--
-- The pipeline STRUCTURE is fixed. The SCORING is the site.
sybilPipeline :: Semiring g
              => (Transfers -> g)   -- ^ Score amounts (forward)
              -> (Double -> g)      -- ^ Score timing (forward)
              -> g                  -- ^ Amount feedback grade (backward)
              -> g                  -- ^ Timing feedback grade (backward)
              -> GradedOptic g Transfers Transfers Bool Bool
sybilPipeline fwdAmt fwdTim bwdAmt bwdTim =
  amtStage |>>>| timStage
  where
    amtStage = fromLens
      (\t -> (amountSim t, fwdAmt t))
      (\t _ -> (t, bwdAmt))
    timStage = fromLens
      (\sim -> (sim > 0.8, fwdTim sim))
      (\_ _ -> (0.5 :: Double, bwdTim))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 1: Log Double — Bayesian bidirectional inference
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

logPipeline :: GradedOptic (Log Double) Transfers Transfers Bool Bool
logPipeline = sybilPipeline
  (\t -> Exp (negate (1 - amountSim t) * 2))  -- similarity → likelihood
  (\sim -> Exp (negate (1 - sim)))             -- timing suspicion
  (Exp (negate 0.5))                           -- backward: update cost
  (Exp (negate 1.2))                           -- backward: model revision cost

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 2: GF(3) — Ternary bidirectional classifier
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

gf3Pipeline :: GradedOptic GF3 Transfers Transfers Bool Bool
gf3Pipeline = sybilPipeline
  (\t -> let s = amountSim t
         in if s > 0.85 then Plus else if s > 0.70 then Zero else Minus)
  (\sim -> if sim > 0.8 then Plus else Minus)
  Plus     -- backward: amount feedback reinforces
  Minus    -- backward: timing feedback corrects (organic signal)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 3: Tropical — Min-cost bidirectional path
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

tropPipeline :: GradedOptic (Tropical Double) Transfers Transfers Bool Bool
tropPipeline = sybilPipeline
  (\t -> Tropical (fromIntegral (length (amounts t)) * 1.5))  -- cost per amount
  (\_ -> Tropical 2.0)                                         -- timing check cost
  (Tropical 3.0)                                               -- backward: recalibration
  (Tropical 5.0)                                               -- backward: model update

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 4: Polynomial — Bidirectional operation accounting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

polyPipeline :: GradedOptic (Poly Int) Transfers Transfers Bool Bool
polyPipeline = sybilPipeline
  (\t -> Poly [0, length (amounts t)])  -- n first-order ops (comparisons)
  (\_ -> Poly [0, 0, 1])                -- one second-order op (correlation)
  (Poly [0, 1])                          -- backward: one first-order update
  (Poly [1])                             -- backward: one zeroth-order reset

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Cybernetic System: Graded Sybil Agent
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A cybernetic sybil detector: internal state = suspicion threshold.
-- Play: classify based on threshold, graded by confidence.
-- Coplay: adjust threshold from feedback, graded by learning cost.
sybilAgent :: Cyber GF3 Transfers Bool
sybilAgent = Cyber
  (0.8 :: Double)                  -- initial suspicion threshold
  (\threshold obs ->
    let s = amountSim obs
        verdict = s > threshold && closes obs
        grade = if verdict then Plus else Minus
    in (verdict, grade))
  (\threshold _obs feedback ->
    let (threshold', grade) = if feedback
          then (threshold - 0.05, Plus)   -- confirmed sybil → lower threshold, sybil grade
          else (threshold + 0.05, Minus)  -- organic → raise threshold, organic grade
    in (threshold', grade))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Main
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

main :: IO ()
main = do
  putStrLn "graded-bayes: Four Sites, Two Directions, One Topos"
  putStrLn "=================================================\n"
  putStrLn "Pipeline: amount analysis |>>>| timing analysis"
  putStrLn "Observation: alice →100→ bob →95→ carol →90→ alice (round-trip)"
  putStrLn $ "Amount similarity: " ++ show (amountSim sample)
  putStrLn "Feedback: False (organic correction)\n"

  let feedback = False

  -- Site 1: Log Double
  let (verdict1, _, gf1, gb1) = runOptic logPipeline sample feedback
  putStrLn "━━━ Site 1: Log Double (Bayesian bidirectional inference) ━━━"
  putStrLn $ "  Forward  (observation): " ++ show gf1
  putStrLn $ "    ln(forward):          " ++ show (ln gf1)
  putStrLn $ "  Backward (feedback):    " ++ show gb1
  putStrLn $ "    ln(backward):         " ++ show (ln gb1)
  putStrLn $ "  Total bidirectional:    " ++ show (gf1 <.> gb1)
  putStrLn $ "  Verdict: " ++ show verdict1
  putStrLn ""

  -- Site 2: GF(3)
  let (_verdict2, _, gf2, gb2) = runOptic gf3Pipeline sample feedback
  let total2 = gf2 <.> gb2
  putStrLn "━━━ Site 2: GF(3) (ternary bidirectional classifier) ━━━"
  putStrLn $ "  Forward  (observation): " ++ show gf2 ++ " (" ++ show (toInt gf2) ++ ")"
  putStrLn $ "  Backward (feedback):    " ++ show gb2 ++ " (" ++ show (toInt gb2) ++ ")"
  putStrLn $ "  Total bidirectional:    " ++ show total2 ++ " (" ++ show (toInt total2) ++ ")"
  putStrLn $ case total2 of
    Plus  -> "  → SYBIL: forward signal persists through feedback"
    Zero  -> "  → AMBIGUOUS: observation and feedback conflict"
    Minus -> "  → ORGANIC: feedback overrides detection"
  putStrLn ""

  -- Site 3: Tropical
  let (_verdict3, _, gf3, gb3) = runOptic tropPipeline sample feedback
  putStrLn "━━━ Site 3: Tropical (min-cost bidirectional path) ━━━"
  putStrLn $ "  Forward  (observation): " ++ show (getTropical gf3) ++ " cost units"
  putStrLn $ "  Backward (feedback):    " ++ show (getTropical gb3) ++ " cost units"
  putStrLn $ "  Total bidirectional:    " ++ show (getTropical (gf3 <.> gb3)) ++ " cost units"
  putStrLn "  → Total = forward + backward (Tropical × = +)"
  putStrLn ""

  -- Site 4: Polynomial
  let (_verdict4, _, gf4, gb4) = runOptic polyPipeline sample feedback
  let total4 = gf4 <.> gb4
  putStrLn "━━━ Site 4: Polynomial (bidirectional operation accounting) ━━━"
  putStrLn $ "  Forward  (observation): " ++ showPoly gf4
  putStrLn $ "  Backward (feedback):    " ++ showPoly gb4
  putStrLn $ "  Total bidirectional:    " ++ showPoly total4
  putStrLn $ "  Evaluated at x=1:       " ++ show (evaluate total4 1) ++ " composite operations"
  putStrLn ""

  -- Cybernetic agent demo
  putStrLn "━━━ Cybernetic Agent (GF3 graded play/coplay) ━━━"
  let (act1, agf1, agb1, agent1) = stepCyber sybilAgent sample True
  putStrLn $ "  Step 1: obs=sample, feedback=True (confirmed sybil)"
  putStrLn $ "    Action: " ++ show act1
  putStrLn $ "    Play grade:   " ++ show agf1 ++ " (" ++ show (toInt agf1) ++ ")"
  putStrLn $ "    Coplay grade: " ++ show agb1 ++ " (" ++ show (toInt agb1) ++ ")"
  let (act2, agf2, agb2, _) = stepCyber agent1 sample False
  putStrLn $ "  Step 2: obs=sample, feedback=False (organic correction)"
  putStrLn $ "    Action: " ++ show act2
  putStrLn $ "    Play grade:   " ++ show agf2 ++ " (" ++ show (toInt agf2) ++ ")"
  putStrLn $ "    Coplay grade: " ++ show agb2 ++ " (" ++ show (toInt agb2) ++ ")"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "The pipeline is fixed: amount analysis |>>>| timing analysis."
  putStrLn "Forward grades compose going IN. Backward grades compose OUT."
  putStrLn "The semiring is the site. The optic is the classifying topos."
  putStrLn "Four sites. Two directions. One structure."

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Display Helpers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
