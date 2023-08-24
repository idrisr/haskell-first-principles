module FractionTests (fractionTests) where

import Data.Ratio ((%))
import Text.Fraction
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Text.Trifecta
import Util

fractionTests :: TestTree
fractionTests = testGroup "Fractions" [unitTests]

unitTests :: TestTree
unitTests =
    let pf = parseString parseFraction mempty
     in testGroup
            "Unit Tests"
            [ expectFail $ testCase "1" $ maybeSuccess (pf badFraction) @?= Nothing
            , testCase "2" $ maybeSuccess (pf "1/1") @?= Just (1 % 1)
            , expectFail $ testCase "" $ maybeSuccess (pf "asdf/1") @?= Just (1 % 1)
            , testCase "" $ maybeSuccess (pf "69/1") @?= Just (69 % 1)
            ]
