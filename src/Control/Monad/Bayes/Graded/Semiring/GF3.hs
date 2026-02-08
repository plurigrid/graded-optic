-- | GF(3) = Z/3Z — the balanced ternary field {-1, 0, +1}.
--
-- Used for sybil detection grading:
--   Plus  (+1) = generator / sloppy
--   Zero  ( 0) = ergodic / ambiguous
--   Minus (-1) = validator / organic
--
-- Conservation law: Σ trits ≡ 0 (mod 3).
module Control.Monad.Bayes.Graded.Semiring.GF3
  ( GF3 (..)
  , toInt
  , fromInt
  ) where

import Control.Monad.Bayes.Graded.Semiring.Class

-- | Balanced ternary: {-1, 0, +1} with arithmetic mod 3.
data GF3 = Minus | Zero | Plus
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | Convert to Int representation.
toInt :: GF3 -> Int
toInt Minus = -1
toInt Zero  = 0
toInt Plus  = 1

-- | Convert from Int (mod 3, balanced).
fromInt :: Int -> GF3
fromInt n = case mod n 3 of
  0 -> Zero
  1 -> Plus
  2 -> Minus  -- -1 ≡ 2 (mod 3)
  _ -> Zero   -- unreachable

instance Semiring GF3 where
  zero = Zero
  one  = Plus

  -- Addition mod 3 (balanced representation)
  Zero  <+> x     = x
  x     <+> Zero  = x
  Plus  <+> Plus  = Minus  -- 1+1 = 2 ≡ -1
  Minus <+> Minus = Plus   -- (-1)+(-1) = -2 ≡ 1
  Plus  <+> Minus = Zero   -- 1+(-1) = 0
  Minus <+> Plus  = Zero   -- (-1)+1 = 0

  -- Multiplication mod 3
  Zero  <.> _     = Zero
  _     <.> Zero  = Zero
  Plus  <.> x     = x
  x     <.> Plus  = x
  Minus <.> Minus = Plus   -- (-1)×(-1) = 1
