module Main (main) where

import ExerciseTest
import Test.Tasty

tests :: TestTree
tests = testGroup "" [exerciseTests]

main :: IO ()
main = defaultMain tests
