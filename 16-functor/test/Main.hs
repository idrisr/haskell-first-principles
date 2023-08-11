module Main (main) where

import Test.Tasty
import FuncsTest
import HeavyLiftingTest

mainTests:: TestTree
mainTests = testGroup "main" [funcTests, heavyTest]

main :: IO ()
main = defaultMain mainTests
