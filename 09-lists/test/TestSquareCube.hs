module TestSquareCube where

import SquareCube
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (==>))

testCube :: TestTree
testCube = testGroup "tests" [testA, testB, testC]

testC :: TestTree
testC =
    testGroup
        "C"
        [ testCase "" $ c @?= 15 ]

testB :: TestTree
testB =
    testGroup
        "B"
        [ testCase "" $
            b
                @?= [ (1, 1)
                    , (1, 8)
                    , (1, 27)
                    , (4, 1)
                    , (4, 8)
                    , (4, 27)
                    , (9, 1)
                    , (9, 8)
                    , (9, 27)
                    , (16, 1)
                    , (16, 8)
                    , (16, 27)
                    , (25, 1)
                    , (25, 8)
                    , (25, 27)
                    ]
        ]

testA :: TestTree
testA =
    testGroup
        "A"
        [ testCase "" $
            a
                @?= [ (1, 1)
                    , (1, 8)
                    , (1, 27)
                    , (1, 64)
                    , (1, 125)
                    , (4, 1)
                    , (4, 8)
                    , (4, 27)
                    , (4, 64)
                    , (4, 125)
                    , (9, 1)
                    , (9, 8)
                    , (9, 27)
                    , (9, 64)
                    , (9, 125)
                    , (16, 1)
                    , (16, 8)
                    , (16, 27)
                    , (16, 64)
                    , (16, 125)
                    , (25, 1)
                    , (25, 8)
                    , (25, 27)
                    , (25, 64)
                    , (25, 125)
                    ]
        ]
