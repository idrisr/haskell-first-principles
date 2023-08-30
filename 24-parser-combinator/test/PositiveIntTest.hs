module PositiveIntTest (posIntTests) where

import PositiveInt
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Util

posIntTests :: TestTree
posIntTests =
    testGroup
        "Positive Int"
        [ unitTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "unit"
        [
        let sut = "123"
            got = parseString parseDigit mempty sut
            wot = Just '1'
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "abc"
              got = parseString parseDigit mempty sut
              wot = Nothing
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "123abc"
              got = parseString base10Integer mempty sut
              wot = Just 123
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "abc"
              got = parseString base10Integer mempty sut
              wot = Nothing
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "-123abc"
              got = parseString base10Integer1 mempty sut
              wot = Just $ -123
           in testCase "" $ maybeSuccess got @?= wot
        ]
