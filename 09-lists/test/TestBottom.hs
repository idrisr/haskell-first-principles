module TestBottom where

import Bottom
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Hspec
import Control.Exception

testBottom:: IO [TestTree]
testBottom= testSpecs spec

spec :: Spec
spec = it "" $
      evaluate a `shouldThrow` anyErrorCall
