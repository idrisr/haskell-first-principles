module Main (main) where

import Test.Tasty
import TestOptional
import TestBull
import TestFirst
import TestSemigroup
import TestMonoid

tests :: TestTree
tests = testGroup "all" [optionalTests, bullTests, firstTests, semigroupTests, monoidTests]

main :: IO ()
main = defaultMain tests
