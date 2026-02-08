-- | Graded weighted monad transformer — the core of graded-bayes.
--
-- @GradedWeightedT g m a@ generalizes monad-bayes's @WeightedT m a@
-- by replacing the hardcoded @Log Double@ weight with an arbitrary
-- semiring @g@. When @g = Log Double@, this is isomorphic to @WeightedT@.
module Control.Monad.Bayes.Graded.Weighted
  ( GradedWeightedT (..)
  , runGraded
  , execGraded
  , withGrade
  , applyGrade
  , hoist
  ) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State.Strict (StateT (..), modify')
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Bayes.Class (MonadDistribution (..))
import Control.Monad.Bayes.Graded.Class (GradedFactor (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))

-- | Weighted monad transformer graded by semiring @g@.
-- Internally just @StateT g m a@, accumulating grade via (<.>).
newtype GradedWeightedT g m a = GradedWeightedT
  { unGraded :: StateT g m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (GradedWeightedT g) where
  lift = GradedWeightedT . lift

-- | Run and return both the value and accumulated grade.
runGraded :: (Semiring g, Monad m) => GradedWeightedT g m a -> m (a, g)
runGraded (GradedWeightedT m) = runStateT m one

-- | Run and return only the accumulated grade.
execGraded :: (Semiring g, Monad m) => GradedWeightedT g m a -> m g
execGraded m = snd <$> runGraded m

-- | Run with a custom initial grade.
withGrade :: g -> GradedWeightedT g m a -> GradedWeightedT g m a
withGrade g (GradedWeightedT m) = GradedWeightedT $ StateT $ \_ -> runStateT m g

-- | Apply a grade transformation to the result.
applyGrade :: (Monad m) => (g -> g) -> GradedWeightedT g m ()
applyGrade f = GradedWeightedT $ modify' f

-- | Change the base monad.
hoist :: (forall x. m x -> n x) -> GradedWeightedT g m a -> GradedWeightedT g n a
hoist f (GradedWeightedT (StateT m)) = GradedWeightedT $ StateT $ f . m

-- Scoring: accumulate via semiring multiplication
instance (Semiring g, Monad m) => GradedFactor g (GradedWeightedT g m) where
  gscore w = GradedWeightedT $ modify' (<.> w)

-- Sampling: delegate to underlying MonadDistribution
instance (Semiring g, MonadDistribution m) => MonadDistribution (GradedWeightedT g m) where
  random = lift random
