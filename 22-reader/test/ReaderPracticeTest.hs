module ReaderPracticeTest where

import Test.Tasty
import Test.Tasty.HUnit
import ReaderPractice

practiceTests :: TestTree
practiceTests = testGroup "ReaderPractice" [unitTests, fakeMainTests]

unitTests :: TestTree
unitTests = testGroup "Unit" [
    testCase "" $ x1 @?= Just (6, 9)
  , testCase "" $ x2 @?= Nothing
  , testCase "" $ x3 3 @?= (Just 9, Just 9)
    ]

fakeMainTests :: TestTree
fakeMainTests =
    testGroup
        "fake Main"
        [ let
            got :: Maybe [Integer]
            got = sequenceA [Just 3, Just 2, Just 1]
            wot = Just [3,2,1]
           in
            testCase "" $ got @?= wot
        , let
            got :: [[Integer]]
            got = sequenceA [x, y]
            wot = [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
           in
            testCase "" $ got @?= wot
        , let
            got :: Maybe [Integer]
            got = sequenceA [xs, ys]
            wot = Just [6, 9]
           in
            testCase "" $ got @?= wot
        , let
            got :: Maybe Integer
            got = summed <$> ((,) <$> xs <*> ys)
            wot = Just 15
           in
            testCase "" $ got @?= wot
        , let
            got :: Maybe Integer
            got = fmap summed ((,) <$> xs <*> zs)
            wot = Nothing
           in
            testCase "" $ got @?= wot
        , let
            got :: Bool
            got = bolt 7
            wot = True
           in
            testCase "" $ got @?= wot
        , let
            got :: [Bool]
            got = fmap bolt z
            wot = [True, False, False]
           in
            testCase "" $ got @?= wot
        , let
            js :: [Int->Bool]
            js = [(>3), (<8), even]
            got :: [Bool]
            got = sequenceA js 7
            wot = [True, True, False]
           in
            testCase "" $ got @?= wot
        ]
