module Main (main) where

import BeginningTest
import ReaderPracticeTest
import Test.Tasty

tests :: TestTree
tests = testGroup "main" [
    beginningTests
  , practiceTests
    ]

main :: IO ()
main = defaultMain tests
