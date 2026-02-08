{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Bridge
-- Description : Three sites, one topos — the same model scored three ways
--
-- "You can have different mathematical theories, possibly belonging to
-- different branches of mathematics, that describe in different languages
-- the same structures; but if you want to extract the semantic content,
-- the object that does this job for you is the classifying topos."
--                                          — Olivia Caramello, ICCM 2023
--
-- This example demonstrates the core thesis of graded-bayes:
-- a single probabilistic model, when scored through different semirings,
-- reveals different invariants of the same underlying structure.
--
-- The model is fixed. The semiring is the site presentation.
-- The inference API is the classifying topos.
module Main where

import Data.Functor.Identity (Identity, runIdentity)
import Numeric.Log (Log (..))

import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))
import Control.Monad.Bayes.Graded.Semiring.LogDomain ()
import Control.Monad.Bayes.Graded.Semiring.GF3 (GF3 (..), toInt)
import Control.Monad.Bayes.Graded.Semiring.Tropical (Tropical (..))
import Control.Monad.Bayes.Graded.Semiring.Polynomial (Poly (..), evaluate)
import Control.Monad.Bayes.Graded.Class (GradedFactor (..))
import Control.Monad.Bayes.Graded.Weighted (GradedWeightedT, runGraded)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § The Model (syntax — independent of semiring)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A transfer between two wallets. The model scores it.
data Transfer = Transfer
  { from   :: String
  , to     :: String
  , amount :: Double
  , timing :: Double  -- seconds since epoch
  } deriving (Show)

-- | Three transfers forming a potential round-trip: A→B→C→A
sampleTransfers :: [Transfer]
sampleTransfers =
  [ Transfer "alice" "bob"   100.0  1000.0
  , Transfer "bob"   "carol"  95.0  1003.0  -- 5% lost, 3s later
  , Transfer "carol" "alice"  90.0  1005.0  -- round-trip closes
  ]

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 1: Log Double — Bayesian likelihood (probability)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Score transfers by how likely they are to be a sybil round-trip.
-- High score = high suspicion.
scoreLogDomain :: [Transfer] -> GradedWeightedT (Log Double) Identity ()
scoreLogDomain transfers = do
  -- Round-trip signal: amount similarity across the chain
  let amounts = map amount transfers
      meanAmt = sum amounts / fromIntegral (length amounts)
      variance = sum [(a - meanAmt)^(2::Int) | a <- amounts] / fromIntegral (length amounts)
      amountScore = Exp (negate variance / 100.0)  -- Gaussian likelihood

  -- Timing signal: suspiciously fast transfers
  let times = map timing transfers
      totalTime = maximum times - minimum times
      timingScore = Exp (negate totalTime / 10.0)  -- faster = more suspicious

  gscore (amountScore <.> timingScore)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 2: GF(3) — Ternary classification (organic/ambiguous/sybil)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Same transfers, but now the score is a trit.
-- No probabilities, no likelihoods — just a verdict.
scoreGF3 :: [Transfer] -> GradedWeightedT GF3 Identity ()
scoreGF3 transfers = do
  -- Round-trip detection: does the chain close?
  let closes = from (head transfers) == to (last transfers)
      roundTripTrit = if closes then Plus else Minus  -- +1 = sybil signal

  -- Amount similarity: within 15%?
  let amounts = map amount transfers
      maxAmt = maximum amounts
      minAmt = minimum amounts
      amountTrit
        | (maxAmt - minAmt) / maxAmt < 0.15 = Plus   -- suspiciously similar
        | (maxAmt - minAmt) / maxAmt < 0.30 = Zero   -- ambiguous
        | otherwise                          = Minus  -- organic divergence

  gscore (roundTripTrit <.> amountTrit)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 3: Tropical — Shortest-path cost (optimization)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Score = total cost of the transfer chain.
-- Tropical multiplication is addition, so gscore accumulates path cost.
scoreTropical :: [Transfer] -> GradedWeightedT (Tropical Double) Identity ()
scoreTropical transfers = do
  -- Each transfer has a cost: amount leaked + gas (timing penalty)
  let costs = zipWith (\t1 t2 ->
        let leaked = amount t1 - amount t2
            gasCost = (timing t2 - timing t1) * 0.01
        in Tropical (leaked + gasCost))
        transfers (tail transfers)
  mapM_ gscore costs

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Site 4: Polynomial — Functor accounting (Weber grade)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Each transfer contributes a monomial tracking the operation type.
-- x^0 = base transfers, x^1 = derived (round-trip closing) transfers.
scorePoly :: [Transfer] -> GradedWeightedT (Poly Int) Identity ()
scorePoly transfers = do
  let n = length transfers
      -- Each transfer is a degree-0 operation (base)
      baseOps = Poly [n]           -- n × x^0
      -- A closing round-trip is a degree-1 operation (composition)
      closes = from (head transfers) == to (last transfers)
      compositeOps = if closes then Poly [0, 1] else Poly [0]  -- x^1 if closes
  gscore (baseOps <.> compositeOps)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § The Bridge
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

main :: IO ()
main = do
  putStrLn "graded-bayes: Three Sites, One Topos"
  putStrLn "====================================\n"
  putStrLn "Transfers: alice →100→ bob →95→ carol →90→ alice (round-trip)\n"

  -- Site 1: Bayesian
  let ((), logWeight) = runIdentity $ runGraded (scoreLogDomain sampleTransfers)
  putStrLn $ "Site 1 — Log Double (Bayesian likelihood):"
  putStrLn $ "  Weight = " ++ show logWeight
  putStrLn $ "  ln(weight) = " ++ show (ln logWeight)
  putStrLn ""

  -- Site 2: Ternary
  let ((), gf3Weight) = runIdentity $ runGraded (scoreGF3 sampleTransfers)
  putStrLn $ "Site 2 — GF(3) (ternary classification):"
  putStrLn $ "  Verdict = " ++ show gf3Weight ++ " (" ++ show (toInt gf3Weight) ++ ")"
  putStrLn $ case gf3Weight of
    Plus  -> "  → SYBIL: round-trip + similar amounts"
    Zero  -> "  → AMBIGUOUS: signals cancel"
    Minus -> "  → ORGANIC: no sybil indicators"
  putStrLn ""

  -- Site 3: Tropical
  let ((), tropWeight) = runIdentity $ runGraded (scoreTropical sampleTransfers)
  putStrLn $ "Site 3 — Tropical (shortest-path cost):"
  putStrLn $ "  Total path cost = " ++ show (getTropical tropWeight)
  putStrLn ""

  -- Site 4: Polynomial
  let ((), polyWeight) = runIdentity $ runGraded (scorePoly sampleTransfers)
  putStrLn $ "Site 4 — Polynomial (functor accounting):"
  putStrLn $ "  Grade polynomial = " ++ showPoly polyWeight
  putStrLn $ "  Evaluated at x=1: " ++ show (evaluate polyWeight 1) ++ " total operations"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "The model is fixed. The semiring is the site."
  putStrLn "The inference API is the classifying topos."
  putStrLn "Different sites reveal different invariants"
  putStrLn "of the same underlying structure."

showPoly :: Poly Int -> String
showPoly (Poly []) = "0"
showPoly (Poly cs) = unwords $ zipWith showTerm [0::Int ..] cs
  where
    showTerm _ 0 = ""
    showTerm 0 c = show c
    showTerm 1 1 = "+ x"
    showTerm 1 c = "+ " ++ show c ++ "x"
    showTerm n 1 = "+ x^" ++ show n
    showTerm n c = "+ " ++ show c ++ "x^" ++ show n
