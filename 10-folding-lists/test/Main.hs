module Main (main) where

import Test.Tasty
import TestUnderstandingFolds
import TestFoldPractice

tests :: TestTree
tests = testGroup "all" [foldTests, pracTests]

main :: IO ()
main = defaultMain tests
