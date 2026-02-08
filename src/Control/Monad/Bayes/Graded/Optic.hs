-- | Graded Optics — bidirectional processes with semiring-valued channels.
--
-- Optics in Kl(Writer g): concrete optics where each channel accumulates
-- a semiring grade. Combines:
--
-- 1. Semiring-graded effects (Katsumata 2014; our GradedWeightedT)
-- 2. Profunctor optics (Riley 2018; Clarke, Gibbons, Loregian et al. 2024)
-- 3. Categorical cybernetics (Capucci, Gavranović, Hedges, Rischel 2021)
--
-- A 'GradedOptic' is an optic where each channel (forward\/backward)
-- carries an independent semiring grade. The same bidirectional process,
-- scored through different semirings, reveals different structure.
--
-- Forward grade = observation cost.  Backward grade = intervention cost.
-- Their product = total bidirectional grade.
module Control.Monad.Bayes.Graded.Optic
  ( -- * Core type
    GradedOptic (..)
    -- * Construction
  , graded
  , ungraded
  , fromLens
  , identity
    -- * Composition
  , (|>>>|)
    -- * Elimination
  , runOptic
  , forwardOnly
  , totalGrade
    -- * Para construction (Gavranović 2024)
  , Para (..)
  , runPara
  , stepPara
    -- * Cybernetic systems (Capucci, Hedges et al. 2021)
  , Cyber (..)
  , stepCyber
  , gradeCyber
  ) where

import Control.Monad.Bayes.Graded.Semiring.Class (Semiring (..))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § The Fundamental Type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A graded optic: a bidirectional data accessor where each direction
-- carries a semiring-valued grade.
--
-- The existential @c@ is the /residual/ — what the optic remembers
-- between the forward pass (observation) and backward pass (intervention).
-- This is the coend variable in the categorical optic formula,
-- here instantiated in Kl(Writer g).
--
-- @
-- forward:  s → (c, a, g)    — decompose, accumulate forward grade
-- backward: c → b → (t, g)   — recompose, accumulate backward grade
-- @
--
-- Fibers (special cases):
--
-- * @g ~ ()@: ordinary ungraded optic
-- * @c ~ ()@: graded arrow pair (no context between passes)
-- * @c ~ s@: graded lens (full context = original structure)
-- * @g = Log Double@: log-likelihood accumulation
-- * @g = GF3@: ternary bidirectional classifier
-- * @g = Tropical Double@: min-cost bidirectional path
-- * @g = Poly Int@: operation accounting (forward ops × backward ops)
data GradedOptic g s t a b = forall c. GradedOptic
  (s -> (c, a, g))     -- ^ Forward: observe, grade the observation
  (c -> b -> (t, g))   -- ^ Backward: intervene, grade the intervention

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Construction
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Construct a graded optic from explicit forward\/backward with residual.
graded :: (s -> (c, a, g)) -> (c -> b -> (t, g)) -> GradedOptic g s t a b
graded = GradedOptic

-- | Lift an ungraded optic into the graded setting with unit grades.
ungraded :: Semiring g => (s -> (c, a)) -> (c -> b -> t) -> GradedOptic g s t a b
ungraded fwd bwd = GradedOptic
  (\s -> let (c, a) = fwd s in (c, a, one))
  (\c b -> (bwd c b, one))

-- | Construct from a graded get\/put pair (lens pattern: residual = s).
fromLens :: (s -> (a, g)) -> (s -> b -> (t, g)) -> GradedOptic g s t a b
fromLens get put = GradedOptic
  (\s -> let (a, g) = get s in (s, a, g))
  put

-- | Identity optic: passes through unchanged, grades are 'one'.
identity :: Semiring g => GradedOptic g a a a a
identity = GradedOptic (\a -> ((), a, one)) (\() a -> (a, one))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Composition
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Compose graded optics.  Grades multiply in each direction.
--
-- Forward: outer forward grade <.> inner forward grade
-- Backward: outer backward grade <.> inner backward grade
-- (inner backward runs first, then outer backward — grades compose in reverse)
--
-- This is the key operation: composition respects BOTH the optic
-- structure AND the grading structure simultaneously.
infixr 1 |>>>|
(|>>>|) :: Semiring g
        => GradedOptic g s t a b
        -> GradedOptic g a b x y
        -> GradedOptic g s t x y
GradedOptic f1 b1 |>>>| GradedOptic f2 b2 = GradedOptic fwd bwd
  where
    fwd s = let (c1, a, g1) = f1 s
                (c2, x, g2) = f2 a
            in ((c1, c2), x, g1 <.> g2)
    bwd (c1, c2) y =
      let (b, gInner) = b2 c2 y
          (t, gOuter) = b1 c1 b
      in (t, gOuter <.> gInner)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Elimination
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Run a graded optic: extract focus, produce updated structure,
-- return forward and backward grades separately.
runOptic :: GradedOptic g s t a b -> s -> b -> (a, t, g, g)
runOptic (GradedOptic fwd bwd) s b =
  let (c, a, gf) = fwd s
      (t, gb)    = bwd c b
  in (a, t, gf, gb)

-- | Forward pass only: extract focus with its grade.
forwardOnly :: GradedOptic g s t a b -> s -> (a, g)
forwardOnly (GradedOptic fwd _) s = let (_, a, g) = fwd s in (a, g)

-- | Total bidirectional grade: forward grade × backward grade.
totalGrade :: Semiring g => GradedOptic g s t a b -> s -> b -> g
totalGrade op s b = let (_, _, gf, gb) = runOptic op s b in gf <.> gb

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Para: Parameterized Graded Morphisms
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A parameterized graded morphism with backward channel.
--
-- Extends Gavranović's Para construction (forward: @w → a → b@) with
-- a backward update channel, making it a parametrised lens in Kl(Writer g).
-- Both the forward computation and the parameter update carry semiring grades.
--
-- @
-- forward:  w → a → (b, g)           — compute output, graded
-- backward: w → a → b → (w, g)       — update parameters, graded
-- @
data Para g a b = forall w. Para
  w                          -- ^ Current parameters
  (w -> a -> (b, g))         -- ^ Forward: play, graded
  (w -> a -> b -> (w, g))   -- ^ Backward: learn, graded

-- | Run a forward pass through a Para morphism.
runPara :: Para g a b -> a -> (b, g)
runPara (Para w fwd _) a = fwd w a

-- | Run a full forward-backward step, returning updated Para.
--
-- @feedback@ is the backward signal (e.g., ground truth, gradient, loss).
stepPara :: Para g a b -> a -> b -> (b, g, g, Para g a b)
stepPara (Para w fwd bwd) a feedback =
  let (b, gf)  = fwd w a
      (w', gb) = bwd w a feedback
  in (b, gf, gb, Para w' fwd bwd)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- § Cybernetic Systems
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | A cybernetic system: an optic-shaped agent with graded channels.
--
-- Forward (play): agent observes environment, produces action, graded.
-- Backward (coplay): agent receives feedback, updates state, graded.
--
-- This is the categorical cybernetics framework of Capucci, Gavranović,
-- Hedges, and Rischel (2021), enriched with semiring grades.
data Cyber g obs act = forall st. Cyber
  st                                   -- ^ Internal state
  (st -> obs -> (act, g))              -- ^ Play: observe → act, graded
  (st -> obs -> act -> (st, g))       -- ^ Coplay: feedback → update, graded

-- | Step the cybernetic system: play forward, then learn backward.
stepCyber :: Cyber g obs act -> obs -> act -> (act, g, g, Cyber g obs act)
stepCyber (Cyber st play coplay) obs feedback =
  let (act, gf) = play st obs
      (st', gb) = coplay st obs feedback
  in (act, gf, gb, Cyber st' play coplay)

-- | Total grade of a cybernetic system's response.
gradeCyber :: Semiring g => Cyber g obs act -> obs -> act -> g
gradeCyber sys obs fb =
  let (_, gf, gb, _) = stepCyber sys obs fb
  in gf <.> gb
