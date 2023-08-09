module Main (main) where

import TestBinaryTree
import Test.Tasty

tests :: TestTree
tests = testGroup "tests" [binaryTreeTests]

main :: IO ()
main = defaultMain tests
