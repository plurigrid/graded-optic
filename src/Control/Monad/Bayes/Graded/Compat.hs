-- | Backward compatibility with monad-bayes WeightedT.
--
-- Provides bidirectional conversion between:
--   @GradedWeightedT (Log Double) m a@ ≅ @WeightedT m a@
--
-- Both are StateT (Log Double) m a under the hood, so these conversions
-- are structurally trivial (run one, re-wrap in the other).
module Control.Monad.Bayes.Graded.Compat
  ( toWeighted
  , fromWeighted
  , liftScoreToGraded
  ) where

import Numeric.Log (Log (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Control.Monad.Bayes.Weighted (WeightedT, weightedT, runWeightedT)
import Control.Monad.Bayes.Graded.Weighted (GradedWeightedT (..))

-- | Convert a graded computation (at Log Double) to monad-bayes WeightedT.
toWeighted :: Monad m => GradedWeightedT (Log Double) m a -> WeightedT m a
toWeighted (GradedWeightedT (StateT f)) = weightedT $ f 1  -- start from one = 1

-- | Convert a monad-bayes WeightedT to a graded computation.
fromWeighted :: Monad m => WeightedT m a -> GradedWeightedT (Log Double) m a
fromWeighted wt = GradedWeightedT $ StateT $ \s -> do
  (a, w) <- runWeightedT wt
  pure (a, s * w)

-- | Lift a standard monad-bayes @score@ call into a graded context.
-- This is the bridge: @liftScoreToGraded w = gscore w@.
liftScoreToGraded :: (Monad m) => Log Double -> GradedWeightedT (Log Double) m ()
liftScoreToGraded w = GradedWeightedT $ StateT $ \s -> pure ((), s * w)
