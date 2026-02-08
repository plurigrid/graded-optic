{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Cybernetic
-- Description : Adaptive agent learns from 8 observations, 3-stage pipeline, Para learner
--
-- The novel combination: graded effects + profunctor optics + categorical cybernetics.
--
-- Three demonstrations:
--
--   1. Three-stage graded pipeline: amount → timing → topology analysis.
--      Run through all 4 semirings, showing how grades compose through
--      three stages instead of two.
--
--   2. Cybernetic agent adapts over 8 observations mixing sybil and organic.
--      Shows threshold evolution, grade trajectories, and how the agent's
--      internal state converges to a regime boundary.
--
--   3. Para learner: parameterized morphism that updates weights from
--      feedback, graded in both forward (prediction cost) and backward
--      (learning cost) channels.
module Main where

import Numeric.Log (Log (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()
import Control.Monad.Bayes.Graded.Semiring.GF3 (GF3 (..))
import Control.Monad.Bayes.Graded.Semiring.Tropical (Tropical (..))
import Control.Monad.Bayes.Graded.Semiring.Polynomial (Poly (..))
import Control.Monad.Bayes.Graded.Optic

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Observation Data
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Observation = Observation
  { obsName     :: String
  , amounts     :: [Double]
  , timingGap   :: Double    -- seconds between transfers
  , closes      :: Bool      -- does the chain form a cycle?
  , selfLoops   :: Int       -- number of self-transfers
  , groundTruth :: Bool      -- True = actually sybil, False = organic
  } deriving Show

-- | 8 observations: a realistic mix of sybil rings, organic chains,
-- wash trades, and ambiguous cases.
observations :: [Observation]
observations =
  [ Observation "sybil-ring"      [100,95,90]         3.0  True  0 True
  , Observation "organic-trade"   [500,50,12]      6500.0  False 0 False
  , Observation "wash-trade"      [1000,1000]         0.1  True  2 True
  , Observation "legit-payment"   [250]            3600.0  False 0 False
  , Observation "bot-cluster"     [100,100,100,100]   0.5  True  0 True
  , Observation "salary-chain"    [5000,2000,800]  86400.0 False 0 False
  , Observation "layered-sybil"   [100,98,97,95]     10.0  True  0 True
  , Observation "airdrop-claim"   [0.01,0.01,0.01]    1.0  False 0 False
  ]

amountSim :: Observation -> Double
amountSim obs =
  let mx = maximum (amounts obs)
      mn = minimum (amounts obs)
  in if mx == 0 then 1.0 else 1.0 - (mx - mn) / mx

timingSuspicion :: Observation -> Double
timingSuspicion obs = 1.0 / (1.0 + timingGap obs / 10.0)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Three-Stage Pipeline (amount → timing → topology)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Generic 3-stage pipeline, polymorphic over the grading semiring.
-- Each stage has its own forward and backward grades.
pipeline3 :: Semiring g
          => (Observation -> g)   -- ^ score amounts (fwd)
          -> (Double -> g)        -- ^ score timing (fwd)
          -> (Bool -> g)          -- ^ score topology (fwd)
          -> g -> g -> g          -- ^ backward grades (amt, tim, topo)
          -> GradedOptic g Observation Observation Bool Bool
pipeline3 fA fT fC bA bT bC =
  amtStage |>>>| timStage |>>>| topoStage
  where
    amtStage = fromLens
      (\obs -> (amountSim obs, fA obs))
      (\obs _ -> (obs, bA))
    timStage = fromLens
      (\sim -> (timingSuspicion' sim, fT sim))
      (\_ _ -> (0.5 :: Double, bT))
    topoStage = fromLens
      (\susp -> (susp > 0.5, fC (susp > 0.5)))
      (\_ _ -> (0.5 :: Double, bC))
    -- Use amount similarity as proxy for timing suspicion in stage 2
    timingSuspicion' sim = sim  -- high similarity → high suspicion

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site-Specific Pipelines
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

logPipe :: GradedOptic (Log Double) Observation Observation Bool Bool
logPipe = pipeline3
  (\obs -> Exp (negate (1 - amountSim obs) * 3))
  (\sim -> Exp (negate (1 - sim)))
  (\c -> if c then Exp 0.5 else Exp (negate 0.5))
  (Exp (negate 0.3))
  (Exp (negate 0.8))
  (Exp (negate 0.2))

gf3Pipe :: GradedOptic GF3 Observation Observation Bool Bool
gf3Pipe = pipeline3
  (\obs -> let s = amountSim obs
           in if s > 0.85 then Plus else if s > 0.5 then Zero else Minus)
  (\sim -> if sim > 0.8 then Plus else if sim > 0.5 then Zero else Minus)
  (\c -> if c then Plus else Minus)
  Plus Minus Plus

tropPipe :: GradedOptic (Tropical Double) Observation Observation Bool Bool
tropPipe = pipeline3
  (\obs -> Tropical (fromIntegral (length (amounts obs)) * 1.5))
  (\_ -> Tropical 2.0)
  (\c -> Tropical (if c then 0.5 else 3.0))
  (Tropical 2.0)
  (Tropical 4.0)
  (Tropical 1.0)

polyPipe :: GradedOptic (Poly Int) Observation Observation Bool Bool
polyPipe = pipeline3
  (\obs -> Poly [0, length (amounts obs)])
  (\_ -> Poly [0, 0, 1])
  (\c -> if c then Poly [0, 0, 0, 1] else Poly [1])
  (Poly [0, 1])
  (Poly [1])
  (Poly [0, 1])

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Cybernetic Agent: Adaptive Sybil Detector
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | An agent that adjusts its suspicion threshold over time.
-- Play: classify observation → sybil/organic verdict, graded by confidence.
-- Coplay: receive ground truth feedback → adjust threshold, graded by learning cost.
adaptiveAgent :: Double -> Cyber GF3 Observation Bool
adaptiveAgent threshold = Cyber
  threshold
  -- Play: observe → decide
  (\th obs ->
    let s = amountSim obs
        verdict = s > th && (closes obs || selfLoops obs > 0)
        grade = if verdict then Plus else Minus
    in (verdict, grade))
  -- Coplay: learn from feedback
  (\th obs truth ->
    let predicted = amountSim obs > th && (closes obs || selfLoops obs > 0)
        (th', grade)
          | truth && not predicted = (th - 0.05, Minus)  -- missed sybil → lower threshold, organic learning grade
          | not truth && predicted = (th + 0.05, Plus)   -- false positive → raise threshold, sybil correction grade
          | truth && predicted     = (th - 0.01, Plus)   -- correct sybil → slight tighten, reinforce
          | otherwise              = (th + 0.01, Minus)  -- correct organic → slight relax
    in (th', grade))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Para Learner: Weighted Sybil Scorer
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A Para morphism: weights determine how to combine signals.
-- Forward: predict sybil score. Backward: update weights from error.
sybilLearner :: Para GF3 Observation GF3
sybilLearner = Para
  (0.5 :: Double, 0.3 :: Double)  -- initial weights: (amount_weight, closure_weight)
  -- Forward: weighted combination → GF3 verdict
  (\(wa, wc) obs ->
    let amtSignal = amountSim obs * wa
        closureSignal = (if closes obs then 1.0 else 0.0) * wc
        combined = amtSignal + closureSignal
        verdict = if combined > 0.6 then Plus else if combined > 0.3 then Zero else Minus
    in (verdict, verdict))  -- grade = the prediction itself
  -- Backward: adjust weights toward correct answer
  (\(wa, wc) obs feedback ->
    let err = case feedback of { Plus -> 1.0; Zero -> 0.0; Minus -> -1.0 :: Double }
        predicted = amountSim obs * wa + (if closes obs then 1.0 else 0.0) * wc
        errSignal = err - predicted
        wa' = wa + 0.1 * errSignal * amountSim obs
        wc' = wc + 0.1 * errSignal * (if closes obs then 1.0 else 0.0)
        grade = if abs errSignal < 0.3 then Plus else Minus
    in ((wa', wc'), grade))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Main
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

main :: IO ()
main = do
  putStrLn "graded-optic: Three-Stage Pipeline × Adaptive Agent × Para Learner"
  putStrLn "==================================================================\n"

  -- ── Part 1: Three-stage pipeline across all observations ──
  putStrLn "PART 1: Three-stage pipeline (amount |>>>| timing |>>>| topology)"
  putStrLn "─────────────────────────────────────────────────────────────────\n"

  putStrLn "  Observation      | Bayesian  | GF(3)   | Tropical | Polynomial"
  putStrLn "  ─────────────────+───────────+─────────+──────────+───────────"

  mapM_ (\obs -> do
    let feedback = groundTruth obs
    let (_,_,gfL,gbL) = runOptic logPipe obs feedback
    let (_,_,gf3,gb3) = runOptic gf3Pipe obs feedback
    let (_,_,gfT,gbT) = runOptic tropPipe obs feedback
    let (_,_,gfP,gbP) = runOptic polyPipe obs feedback

    let totalL = gfL <.> gbL
    let total3 = gf3 <.> gb3
    let totalT = gfT <.> gbT
    let totalP = gfP <.> gbP

    putStrLn $ "  " ++ pad 17 (obsName obs)
      ++ " | " ++ pad 9 (take 9 $ show (ln totalL))
      ++ " | " ++ pad 7 (showGF3Short total3)
      ++ " | " ++ pad 8 (take 8 $ show (getTropical totalT))
      ++ " | " ++ showPoly totalP
    ) observations

  putStrLn ""

  -- ── Part 2: Cybernetic agent learning over 8 steps ──
  putStrLn "PART 2: Cybernetic agent adapts over 8 observations"
  putStrLn "───────────────────────────────────────────────────\n"

  putStrLn "  Step | Observation      | Verdict | Truth | Play | Coplay | Threshold"
  putStrLn "  ─────+──────────────────+─────────+───────+──────+────────+──────────"

  let runSteps _ _ [] = return ()
      runSteps agent th ((i, obs):rest) = do
        let (verdict, playG, coplayG, agent') =
              stepCyber agent obs (groundTruth obs)
        -- Compute new threshold from the learning rule
        let predicted = amountSim obs > th && (closes obs || selfLoops obs > 0)
            th' | groundTruth obs && not predicted = th - 0.05
                | not (groundTruth obs) && predicted = th + 0.05
                | groundTruth obs && predicted       = th - 0.01
                | otherwise                          = th + 0.01
        putStrLn $ "    " ++ pad 2 (show (i :: Int))
          ++ "  | " ++ pad 16 (obsName obs)
          ++ " | " ++ pad 7 (if verdict then "SYBIL" else "ORGANIC")
          ++ " | " ++ pad 5 (if groundTruth obs then "sybil" else "clean")
          ++ " | " ++ pad 4 (showGF3Short playG)
          ++ " | " ++ pad 6 (showGF3Short coplayG)
          ++ " | " ++ take 4 (show th')
        let correct = verdict == groundTruth obs
        if correct then putStr "" else putStrLn $ "       ↑ WRONG — agent adjusts"
        runSteps agent' th' rest

  runSteps (adaptiveAgent 0.80) 0.80 (zip [1..] observations)
  putStrLn ""

  -- ── Part 3: Para learner weight evolution ──
  putStrLn "PART 3: Para learner — weight evolution over observations"
  putStrLn "────────────────────────────────────────────────────────\n"

  putStrLn "  Step | Observation      | Predicted | Feedback | Fwd Grade | Bwd Grade"
  putStrLn "  ─────+──────────────────+───────────+──────────+───────────+──────────"

  let runPara' _ [] = return ()
      runPara' p ((i, obs):rest) = do
        let truthGrade = if groundTruth obs then Plus else Minus
        let (predicted, gf, gb, p') = stepPara p obs truthGrade
        putStrLn $ "    " ++ pad 2 (show (i :: Int))
          ++ "  | " ++ pad 16 (obsName obs)
          ++ " | " ++ pad 9 (showGF3Short predicted)
          ++ " | " ++ pad 8 (showGF3Short truthGrade)
          ++ " | " ++ pad 9 (showGF3Short gf)
          ++ " | " ++ showGF3Short gb
        runPara' p' rest

  runPara' sybilLearner (zip [1..] observations)
  putStrLn ""

  -- ── Coda ──
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Three stages compose: amount |>>>| timing |>>>| topology."
  putStrLn "Forward grades compose going IN. Backward grades compose OUT."
  putStrLn "The Cyber agent's play/coplay grades track prediction confidence"
  putStrLn "and learning cost as GF(3) trits — one semiring multiplication"
  putStrLn "per feedback step."
  putStrLn ""
  putStrLn "The Para learner updates weights from error, with both forward"
  putStrLn "(prediction) and backward (update) channels independently graded."
  putStrLn ""
  putStrLn "The semiring is the site. The optic is the classifying topos."

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Display Helpers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

showGF3Short :: GF3 -> String
showGF3Short Plus  = "+1"
showGF3Short Zero  = " 0"
showGF3Short Minus = "-1"

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
