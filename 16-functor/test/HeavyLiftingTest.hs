module HeavyLiftingTest where

import HeavyLifting
import Test.Tasty.HUnit
import Test.Tasty

heavyTest :: TestTree
heavyTest = testGroup "heavy" [
    testCase "A" $ heavyA  @?= [2]
  , testCase "B" $ heavyB  @?= Just ["Hi,lol","Hellolol"]
  , testCase "C" $ heavyC 1 @?= (-2)
  , testCase "D" $ heavyD 0 @?= "1[0,1,2,3]"
    ]
