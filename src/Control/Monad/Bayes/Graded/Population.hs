-- | Graded population monad transformer — a collection of graded weighted particles.
--
-- @GradedPopulationT g m a@ maintains a population of particles, each carrying
-- a semiring-valued weight. This is the graded analogue of monad-bayes's @PopulationT@.
module Control.Monad.Bayes.Graded.Population
  ( GradedPopulationT (..)
  , fromWeightedList
  , spawn
  , resampleGeneric
  ) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Bayes.Class (MonadDistribution (..))
import Control.Monad.Bayes.Graded.Class (GradedFactor (..))
import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))

-- | A population of particles, each with a semiring-valued weight.
-- Represented as m [(a, g)] — a monadic computation producing weighted particles.
newtype GradedPopulationT g m a = GradedPopulationT
  { runPopulation :: m [(a, g)] }

instance (Semiring g, Monad m) => Functor (GradedPopulationT g m) where
  fmap f (GradedPopulationT m) = GradedPopulationT $ do
    ps <- m
    pure [(f a, w) | (a, w) <- ps]

instance (Semiring g, Monad m) => Applicative (GradedPopulationT g m) where
  pure a = GradedPopulationT $ pure [(a, one)]
  GradedPopulationT mf <*> GradedPopulationT mx = GradedPopulationT $ do
    fs <- mf
    xs <- mx
    pure [(f a, wf <.> wx) | (f, wf) <- fs, (a, wx) <- xs]

instance (Semiring g, Monad m) => Monad (GradedPopulationT g m) where
  GradedPopulationT mx >>= f = GradedPopulationT $ do
    xs <- mx
    concat <$> mapM (\(a, w) -> do
      ys <- runPopulation (f a)
      pure [(b, w <.> w') | (b, w') <- ys]) xs

instance (Semiring g) => MonadTrans (GradedPopulationT g) where
  lift m = GradedPopulationT $ do
    a <- m
    pure [(a, one)]

instance (Semiring g, Monad m) => GradedFactor g (GradedPopulationT g m) where
  gscore w = GradedPopulationT $ pure [((), w)]

instance (Semiring g, MonadDistribution m) => MonadDistribution (GradedPopulationT g m) where
  random = lift random

-- | Create a population from a weighted list.
fromWeightedList :: Monad m => m [(a, g)] -> GradedPopulationT g m a
fromWeightedList = GradedPopulationT

-- | Spawn n copies of each particle (for initialization).
spawn :: (Semiring g, Monad m) => Int -> GradedPopulationT g m ()
spawn n = GradedPopulationT $ pure $ replicate n ((), one)

-- | Generic resampling: given a function that selects indices from
-- a list of weights, produce a resampled population.
resampleGeneric ::
  (Semiring g, MonadDistribution m) =>
  ([g] -> m [Int]) ->
  GradedPopulationT g m a ->
  GradedPopulationT g m a
resampleGeneric selector (GradedPopulationT m) = GradedPopulationT $ do
  ps <- m
  let ws = map snd ps
  indices <- selector ws
  pure [(fst (ps !! i), one) | i <- indices]
