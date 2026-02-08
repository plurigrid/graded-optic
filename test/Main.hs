module Main (main) where

import Test.Tasty
import SemiringLaws (semiringTests)
import Isomorphism (isomorphismTests)
import OpticLaws (opticTests)

main :: IO ()
main = defaultMain $ testGroup "graded-bayes"
  [ semiringTests
  , isomorphismTests
  , opticTests
  ]
