module LibFuncsTest (funcTests) where

import Data.Monoid
import LibFuncs
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (elem, product, sum)

funcTests :: TestTree
funcTests =
    testGroup
        "Funcs"
        [ sumTests
        , productTests
        ]

sumTests :: TestTree
sumTests =
    testGroup
        "Sum"
        [ let
            xs :: [Sum Int]
            xs = fmap Sum [1 .. 10]
            got = sum xs
            wot = 55
           in
            testCase "Sum List" $ got @?= wot
        , let
            xs :: Maybe (Sum Int)
            xs = Just 10
            got = product xs
            wot = 10
           in
            testCase "Sum Maybe" $ got @?= wot
        , let
            xs :: Maybe (Product Int)
            xs = Nothing
            got = product xs
            wot = 1
           in
            testCase "Product Maybe Nothing" $ got @?= wot
        ]

productTests :: TestTree
productTests =
    testGroup
        "Product"
        [ let
            xs :: [Product Int]
            xs = fmap Product [1 .. 5]
            got = product xs
            wot = 120
           in
            testCase "Product List" $ got @?= wot
        , let
            xs :: Maybe (Product Int)
            xs = Just 10
            got = product xs
            wot = 10
           in
            testCase "Product Maybe" $ got @?= wot
        , let
            xs :: Maybe (Product Int)
            xs = Nothing
            got = product xs
            wot = 1
           in
            testCase "Product Maybe Nothing" $ got @?= wot
        ]
