module TestComprehension where

import Comprehension
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (==>))

testComprehension :: TestTree
testComprehension = testGroup "tests" [testA, testB, testC]

testA :: TestTree
testA =
    testGroup
        "A"
        [ testCase "" $ a @?= [] ]

testB :: TestTree
testB =
    testGroup
        "B"
        [ testCase "" $ b @?= [
            (1, 64),
            (1, 81),
            (1, 100),
            (4, 64),
            (4, 81),
            (4, 100),
            (9, 64),
            (9, 81),
            (9, 100),
            (16, 64),
            (16, 81),
            (16, 100),
            (25, 64),
            (25, 81),
            (25, 100),
            (36, 64),
            (36, 81),
            (36, 100),
            (49, 64),
            (49, 81),
            (49, 100)
            ]
        ]

testC :: TestTree
testC =
    testGroup
        "C"
        [ testCase "" $ c @?= [
            (1, 64),
            (1, 81),
            (1, 100),
            (4, 64),
            (4, 81)
            ]
        ]
