module Main (main) where

import Test.Tasty
import TestUnderstandingFolds

main :: IO ()
main = defaultMain foldTests
