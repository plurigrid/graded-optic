-- | QualifiedDo support for graded monadic syntax.
--
-- Usage with @QualifiedDo@:
--
-- @
-- {-# LANGUAGE QualifiedDo #-}
-- import qualified Control.Monad.Bayes.Graded.Do as G
--
-- model :: (GradedMeasure g m) => m Double
-- model = G.do
--   x <- random
--   gscore (f x)
--   pure x
-- @
--
-- This module re-exports (>>=), (>>), and pure with the standard
-- types, making QualifiedDo work transparently with graded monads.
module Control.Monad.Bayes.Graded.Do
  ( (>>=)
  , (>>)
  , pure
  , return
  , fail
  ) where

import Prelude ((>>=), (>>), pure, fail, Monad)

-- | @return = pure@ for backward compatibility.
return :: Monad m => a -> m a
return = pure
