module Util where

import Text.Trifecta
import Test.Tasty
import Test.Tasty.HUnit

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess (Failure _) = Nothing

displayResult :: Result a -> String
displayResult (Success _) = ""
displayResult (Failure e) = show e

parseTestCase :: (Eq a, Show a) => String -> Result a -> Maybe a -> TestTree
parseTestCase sut got wot = testCase sut $ assertParse got wot

assertParse :: (Eq a, Show a) => Result a -> Maybe a -> Assertion
assertParse got wot = assertEqual (showErr got) wot $ maybeSuccess got

showErr :: Result a -> String
showErr (Success _) = ""
showErr (Failure e) = show e
