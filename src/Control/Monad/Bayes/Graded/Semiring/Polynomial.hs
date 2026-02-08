-- | Polynomial semiring N[x] — for functor accounting and polynomial 2-monad grades.
--
-- A polynomial p(x) = a₀ + a₁x + a₂x² + ... with natural number coefficients.
-- Addition is pointwise, multiplication is convolution (Cauchy product).
--
-- Connection to Weber (2015): polynomial functors P(X) = Σ aₙ Xⁿ grade
-- the operations of a colored operad. The semiring structure tracks how
-- operations compose.
module Control.Monad.Bayes.Graded.Semiring.Polynomial
  ( Poly (..)
  , constant
  , monomial
  , degree
  , evaluate
  ) where

import Control.Monad.Bayes.Graded.Semiring.Class

-- | Polynomial with coefficients in a. Stored as a list of coefficients
-- from degree 0 upward: Poly [a₀, a₁, a₂, ...] = a₀ + a₁x + a₂x² + ...
-- Trailing zeros are normalized away.
newtype Poly a = Poly { getCoeffs :: [a] }
  deriving stock (Show, Read)

-- | Equality up to trailing zeros.
instance (Eq a, Num a) => Eq (Poly a) where
  Poly xs == Poly ys = normalize xs == normalize ys

-- | Trim trailing zeros.
normalize :: (Eq a, Num a) => [a] -> [a]
normalize = reverse . dropWhile (== 0) . reverse

-- | Constant polynomial.
constant :: a -> Poly a
constant a = Poly [a]

-- | Monomial aₙxⁿ.
monomial :: Num a => Int -> a -> Poly a
monomial n a = Poly (replicate n 0 ++ [a])

-- | Degree of polynomial (-1 for zero polynomial).
degree :: (Eq a, Num a) => Poly a -> Int
degree (Poly cs) = length (normalize cs) - 1

-- | Evaluate polynomial at a point.
evaluate :: Num a => Poly a -> a -> a
evaluate (Poly cs) x = foldr (\c acc -> c + x * acc) 0 cs

-- | Pointwise addition with length extension.
addCoeffs :: Num a => [a] -> [a] -> [a]
addCoeffs [] ys = ys
addCoeffs xs [] = xs
addCoeffs (x:xs) (y:ys) = (x + y) : addCoeffs xs ys

-- | Cauchy product (convolution).
mulCoeffs :: Num a => [a] -> [a] -> [a]
mulCoeffs [] _ = []
mulCoeffs _ [] = []
mulCoeffs (x:xs) ys = addCoeffs (map (x *) ys) (0 : mulCoeffs xs ys)

instance (Eq a, Num a) => Semiring (Poly a) where
  zero = Poly []
  one  = Poly [1]
  Poly xs <+> Poly ys = Poly (normalize (addCoeffs xs ys))
  Poly xs <.> Poly ys = Poly (normalize (mulCoeffs xs ys))
