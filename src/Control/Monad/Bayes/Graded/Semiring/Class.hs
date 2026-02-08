-- | Semiring typeclass — the minimal algebraic structure for grading.
--
-- A semiring (R, +, ×, 0, 1) satisfies:
--   (R, +, 0) is a commutative monoid
--   (R, ×, 1) is a monoid
--   × distributes over + from both sides
--   0 annihilates: 0 × r = r × 0 = 0
module Control.Monad.Bayes.Graded.Semiring.Class
  ( Semiring (..)
  ) where

-- | A semiring with additive and multiplicative structure.
class Semiring a where
  -- | Additive identity
  zero :: a
  -- | Multiplicative identity
  one  :: a
  -- | Additive operation (commutative, associative)
  (<+>) :: a -> a -> a
  -- | Multiplicative operation (associative, distributes over <+>)
  (<.>) :: a -> a -> a

infixl 6 <+>
infixl 7 <.>
