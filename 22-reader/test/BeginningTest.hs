module BeginningTest where

import Beginning
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

propFmapComp :: (Eq a, Num a) => a -> a -> Bool
propFmapComp x y = (h . f) x y == (h <$> f) x y
    where f = (*2)
          h = (+)

beginningTests :: TestTree
beginningTests = testGroup "" [unitTests, propTests]

unitTests :: TestTree
unitTests = testGroup "" [
    testCase "" $ bbop 3 @?= 19
  , testCase "" $ bbop 4 @?= 22
  ]

propTests :: TestTree
propTests = testGroup "" [
    testProperty ""  (propFmapComp:: Int -> Int -> Bool)
    ]
