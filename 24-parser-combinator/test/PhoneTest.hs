module PhoneTest where

import Phone
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta
import Util

phoneTests :: TestTree
phoneTests =
    testGroup
        "Phone"
        [ unitTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "unit"
        [ let sut = "123-456-7890"
              got = parseString parsePhone mempty sut
              wot = Just $ PhoneNumber 123 456 7890
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "(123) 456-7890"
              got = parseString parsePhone mempty sut
              wot = Just $ PhoneNumber 123 456 7890
           in testCase "" $ maybeSuccess got @?= wot
        , let sut = "1-123-456-7890"
              got = parseString parsePhone mempty sut
              wot = Just $ PhoneNumber 123 456 7890
           in testCase "" $ maybeSuccess got @?= wot
        ]
