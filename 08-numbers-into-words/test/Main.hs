module Main (main) where

import Recursion
import CurryReview
import Division
import McCarthy91

import Test.Tasty.HUnit
import Test.Tasty

tests :: TestTree
tests = testGroup "tests" [unitTests, unitTests2, unitTests3, unitTests4]

unitTests::TestTree
unitTests = testGroup "Currying"
  [
  testCase "1" $ appedCatty "woohoo" @?= "woops mrow woohoo",
  testCase "2" $ frappe "1" @?= "1 mrow haha",
  let a = frappe (appedCatty (frappe "blue"))
      b = "woops mrow blue mrow haha mrow haha" in
  testCase "3" $ a @?= b
  ]

unitTests2 :: TestTree
unitTests2 = testGroup "Math"
  [
  testCase "Sum" $ sUm (5::Integer) @?= 15,
  testCase "Sum" $ sUm (10::Integer) @?= 55,
  testCase "Mult" $ mult 5 (10::Integer) @?= 50,
  testCase "Mult" $ mult 3 (23::Integer) @?= 69,
  testCase "Mult" $ mult 0 (10::Integer) @?= 0,
  testCase "Mult" $ mult 10 (0::Integer) @?= 0,
  testCase "Mult" $ mult 0 (0::Integer) @?= 0
  ]

unitTests3 :: TestTree
unitTests3 = testGroup "DividedBy"
  [
  testCase "" $ (10::Integer) `dividedBy` 2 @?= Result 5,
  testCase "" $ (10::Integer) `dividedBy` (-2) @?= Result (-5),
  testCase "" $ (-10::Integer) `dividedBy` (-2) @?= Result 5,
  testCase "" $ (-10::Integer) `dividedBy` 2 @?= Result (-5),
  testCase "" $ (-10::Integer) `dividedBy` 0 @?= DivideByZero,
  testCase "" $ (10::Integer) `dividedBy` 0 @?= DivideByZero,
  testCase "" $ (0::Integer) `dividedBy` 0 @?= DivideByZero
  ]

unitTests4 :: TestTree
unitTests4 = testGroup "McCarthy91"
  [
  testCase "" $ map mccarthy91 [(95::Integer)..110] @?= [91,91,91,91,91,91,91,92,93,94,95,96,97,98, 99,100]
  ]

main :: IO ()
main = defaultMain tests
