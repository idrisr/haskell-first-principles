module ExerciseTest where

import Exercise
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Moi

exerciseTests :: TestTree
exerciseTests = testGroup "Exercise" [unitTests, propTests]

propExecPut :: Eq a => a -> a -> Bool
propExecPut x y = exec (put x) y == x

propGet :: Eq a => a -> Bool
propGet x = runMoi get x == (x, x)

propExecGet :: Eq a => a -> Bool
propExecGet x = exec get x == x

propEvalGet :: Eq a => a -> Bool
propEvalGet x = eval get x == x

propTests :: TestTree
propTests = testGroup "propTests" [
    testProperty "" (propExecPut :: String -> String -> Bool)
  , testProperty "" (propGet :: [String] -> Bool)
  , testProperty "" (propExecGet :: [Maybe Int] -> Bool)
  , testProperty "" (propEvalGet :: [Either Int Double] -> Bool)
    ]


unitTests :: TestTree
unitTests =
    testGroup
        "unitTests"
        [ let
            got = runMoi get "curryIsAmaze"
            wot = ("curryIsAmaze", "curryIsAmaze")
           in
            testCase "" $ got @?= wot
        , let
            got = exec (put "wilma") "literally anything"
            wot = "wilma"
           in
            testCase "" $ got @?= wot
        , let
            got = exec get "scooby papu"
            wot = "scooby papu"
           in
            testCase "" $ got @?= wot
        , let
            got = eval get "bunnicula"
            wot = "bunnicula"
           in
            testCase "" $ got @?= wot
        , let
            got = eval get "stake a bunny"
            wot = "stake a bunny"
           in
            testCase "" $ got @?= wot
        , let
            f :: Moi Integer ()
            f = modify (+1)
            got = runMoi f 0
            wot = ((), 1)
           in
            testCase "" $ got @?= wot
        , let
            f :: Moi Integer ()
            f = modify (+1)
            got = runMoi $ f >> f
            wot = ((), 2)
           in
            testCase "" $ got 0 @?= wot
        ]
