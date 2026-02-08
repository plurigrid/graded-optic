-- | Tropical semiring: (min, +) over extended reals.
--
-- zero = +∞ (additive identity for min)
-- one  = 0  (multiplicative identity for +)
--
-- Used for Viterbi decoding, shortest-path optimization, and
-- max-likelihood inference (negate for max-plus).
module Control.Monad.Bayes.Graded.Semiring.Tropical
  ( Tropical (..)
  ) where

import Control.Monad.Bayes.Graded.Semiring.Class

-- | Tropical semiring wrapper. The underlying value represents a "cost"
-- or "distance"; (min, +) finds the cheapest path.
newtype Tropical a = Tropical { getTropical :: a }
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, Fractional, Floating, Real, RealFrac, RealFloat)

instance (RealFloat a) => Semiring (Tropical a) where
  zero = Tropical (1/0)   -- +∞
  one  = Tropical 0

  -- Addition = min (choose the shorter path)
  Tropical x <+> Tropical y = Tropical (min x y)

  -- Multiplication = + (concatenate path costs)
  Tropical x <.> Tropical y = Tropical (x + y)
