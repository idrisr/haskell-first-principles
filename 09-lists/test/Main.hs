module Main where

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, (==>))
import Test.QuickCheck (Arbitrary(..), choose, quickCheck)
import EnumDef

tests :: TestTree
tests = testGroup "tests" [testTree]

-- QuickCheck property
property :: (Enum a, Eq a) => (a -> a -> [a]) -> a -> a -> Bool
property f x y = f x y == [x..y]

testTree :: TestTree
testTree = testGroup "Enum Ranges"
  [
  testProperty "" $ \x y -> property eftBool (x::Bool) y,
  testProperty "" $ \x y -> property eftOrd (x::Ordering) y,
  testProperty "" $ \x y -> property eftInt (x::Int) y,
  testProperty "" $ \x y -> property eftChar (x::Char) y
  ]

main :: IO ()
main = defaultMain tests
