module Main (main) where

import ComposeTest
import IdentityTest
import Test.Tasty

tests :: TestTree
tests = testGroup "" [composeTests, identityTests]

main :: IO ()
main = defaultMain tests
