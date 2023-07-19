module Main where

import EnumDef
import Test.QuickCheck (Arbitrary (..), choose, quickCheck)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty, (==>))
import TestComprehension
import TestSquareCube
import TestWords
import TestZip

tests :: TestTree
tests = testGroup "tests" [testTree, wordTests, testComprehension, testCube, qcProps]

-- QuickCheck property
property :: (Enum a, Eq a) => (a -> a -> [a]) -> a -> a -> Bool
property f x y = f x y == [x .. y]

testTree :: TestTree
testTree =
    testGroup
        "Enum Ranges"
        [ testProperty "" $ \x y -> property eftBool (x :: Bool) y
        , testProperty "" $ \x y -> property eftOrd (x :: Ordering) y
        , testProperty "" $ \x y -> property eftInt (x :: Int) y
        , testProperty "" $ \x y -> property eftChar (x :: Char) y
        ]

main :: IO ()
main =  do
    defaultMain tests
