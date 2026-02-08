# graded-optic

Semiring-graded bidirectional processes for Haskell.

## Why

This library combines three existing constructions into a single Haskell type:

1. **Semiring-graded effects** — Katsumata (2014), dorchard/effect-monad.
2. **Profunctor optics** — Riley (2018), Clarke et al. (2024), mroman42/vitrea.
3. **Categorical cybernetics** — Capucci, Gavranović, Hedges, Rischel (2021).

The result: **optics in Kl(Writer g)** — concrete optics where each channel (forward/backward) accumulates a semiring grade. Forward grade = observation cost. Backward grade = intervention cost. The same bidirectional process, scored through different semirings, reveals different structure from both directions.

## The type

```haskell
data GradedOptic g s t a b = forall c. GradedOptic
  (s -> (c, a, g))     -- forward: observe, grade the observation
  (c -> b -> (t, g))   -- backward: intervene, grade the intervention
```

One type. The existential `c` is the residual (the coend variable in the optic formula). The grade `g` is any semiring (Katsumata's parameter). This is a concrete optic in Kl(Writer g). Composition multiplies grades in each direction independently:

```haskell
(|>>>|) :: Semiring g => GradedOptic g s t a b -> GradedOptic g a b x y -> GradedOptic g s t x y
```

Forward grades compose going in. Backward grades compose going out.

## Four semirings, two directions

```
$ cabal run cybernetic-example

Pipeline: amount analysis |>>>| timing analysis
Observation: alice →100→ bob →95→ carol →90→ alice (round-trip)
Amount similarity: 0.9
Feedback: False (organic correction)

━━━ Semiring 1: Log Double (log-likelihood accumulation) ━━━
  Forward  (observation): 0.741
  Backward (feedback):    0.183
  Total bidirectional:    0.135

━━━ Semiring 2: GF(3) (ternary bidirectional classifier) ━━━
  Forward  (observation): Plus (1)
  Backward (feedback):    Minus (-1)
  Total bidirectional:    Minus (-1)
  → ORGANIC: feedback overrides detection

━━━ Semiring 3: Tropical (min-cost bidirectional path) ━━━
  Forward  (observation): 6.5 cost units
  Backward (feedback):    8.0 cost units
  Total bidirectional:    14.5 cost units

━━━ Semiring 4: Polynomial (bidirectional operation accounting) ━━━
  Forward  (observation): 3x³
  Backward (feedback):    x
  Total bidirectional:    3x⁴
```

The GF(3) result is the crux: `Plus × Minus = Minus`. Organic feedback *overrides* sybil detection in the ternary field. One semiring multiplication across the optic's backward channel.

## Also includes

| Module | What |
|--------|------|
| `GradedWeightedT g m a` | Forward-only graded monad (extends [monad-bayes](https://github.com/tweag/monad-bayes)) |
| `GradedOptic g s t a b` | Bidirectional graded optic (optics in Kl(Writer g)) |
| `Para g a b` | Parameterized graded morphism (Gavranović's Para) |
| `Cyber g obs act` | Cybernetic system with graded play/coplay |
| 4 semirings | `Log Double`, `GF3`, `Tropical`, `Polynomial` |
| 37 QuickCheck tests | Semiring laws, isomorphism, optic identity/composition, Para, Cyber |

## Build

Requires GHC ≥ 9.4.

```bash
cabal build
cabal test
cabal run bridge-example       # forward-only: three scenarios × four semirings
cabal run cybernetic-example   # bidirectional: four semirings, two directions
```

## Theory

- Katsumata (2014), *Parametric effect monads and semantics of effect systems*
- Riley (2018), *Categories of optics*
- Clarke, Elkins, Gibbons, Loregian, Milewski, Pillmore, Román (2024), *Profunctor optics: a categorical update*
- Capucci, Gavranović, Hedges, Rischel (2021), *Towards foundations of categorical cybernetics*
- Gavranović (2024), *Fundamental components of deep learning: a category-theoretic approach* (PhD thesis)
- Hedges (2017), *Coherence for lenses and open games*

## License

MIT
