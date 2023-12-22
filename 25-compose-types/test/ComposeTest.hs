module ComposeTest (composeTests) where

import Compose
import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

composeTests :: TestTree
composeTests =
    testGroup
        "Compose"
        [ unitTests
        , foldableTests
        , applicativeTests
        , functorTests
        ]

unitTests :: TestTree
unitTests =
    testGroup
        "unit"
        [ let
            sut :: Compose [] Maybe Int
            sut = Compose [Just 10, Nothing, Just 11]
            wot = Compose [Just 11, Nothing, Just 12]
            x :: Int
            x = 1
            got = fmap (+ x) sut
           in
            testCase "" $ got @?= wot
        , let
            sut :: Compose [] Maybe String
            sut = Compose [Just "10", Nothing, Just "11"]
            got = foldMap (++ "YO") sut
            wot = "10YO11YO"
           in
            testCase "foldMap" $ got @?= wot
        ]

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Compose [] Maybe F
xs = undefined

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [testProperties "" props]

applicativeTests :: TestTree
applicativeTests =
    let props = unbatch $ applicative ys
     in testGroup "Applicative" [testProperties "" props]

type G = (Sum Int, String, Product Int)
ys :: Compose [] Maybe G
ys = undefined

functorTests :: TestTree
functorTests =
    let props = unbatch $ functor ys
     in testGroup "Functor" [testProperties "" props]
