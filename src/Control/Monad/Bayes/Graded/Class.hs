-- | Core typeclasses for graded probabilistic programming.
--
-- The key insight: monad-bayes's MonadFactor hardcodes @score :: Log Double -> m ()@.
-- We generalize to @gscore :: g -> m ()@ where g is any semiring.
-- Sampling (MonadDistribution) is ungraded — randomness has no weight.
module Control.Monad.Bayes.Graded.Class
  ( -- * Graded scoring
    GradedFactor (..)
    -- * Re-export ungraded sampling
  , MonadDistribution (..)
    -- * Graded measure = sampling + graded scoring
  , GradedMeasure
  ) where

import Control.Monad.Bayes.Class (MonadDistribution (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring)

-- | A monad that supports scoring with values from a semiring @g@.
--
-- Laws:
--   gscore one       = pure ()           -- unit
--   gscore a >> gscore b = gscore (a <.> b) -- multiplicative homomorphism
--
-- The functional dependency @m -> g@ ensures each monad stack
-- determines its grade type unambiguously.
class (Semiring g, Monad m) => GradedFactor g m | m -> g where
  -- | Score the current execution with a semiring value.
  -- Accumulated via semiring multiplication (<.>).
  gscore :: g -> m ()

-- | A graded measure monad: can both sample and score.
-- This is the graded analogue of monad-bayes's MonadMeasure.
type GradedMeasure g m = (MonadDistribution m, GradedFactor g m)
