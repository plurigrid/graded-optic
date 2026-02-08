-- | Log-domain Double as a semiring — backward compatible with monad-bayes WeightedT.
--
-- This is the "default" grade: Log Double with log-space addition and multiplication.
-- GradedWeightedT (Log Double) ≅ WeightedT via zero-cost newtype coercion.
module Control.Monad.Bayes.Graded.Semiring.LogDomain
  ( -- * Re-export
    Log (..)
  ) where

import Numeric.Log (Log (..))
import Control.Monad.Bayes.Graded.Semiring.Class

-- | Log Double forms a semiring under (logAdd, *).
-- zero = exp(-∞) = 0, one = exp(0) = 1.
instance (RealFloat a, Ord a) => Semiring (Log a) where
  zero = 0
  one  = 1
  (<+>) = (+)  -- Log domain addition (logAddExp)
  (<.>) = (*)  -- Log domain multiplication (addition of logs)
