module FourTest (fourTests) where

import Four
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

fourTests :: TestTree
fourTests = testGroup "Four" [functorTests, applicativeTests]

xs ::
    Four
        (Sum Int, Product Int, Maybe (Sum Int))
        (Maybe (Sum Int), Sum Int, Product Int)
        (Product Int, Maybe (Sum Int), Sum Int)
        (Sum Int, Product Int, Maybe (Sum Int))
xs = Four a b c d
  where
    a = (420, 1, Nothing)
    b = (Just 68, 20, 11)
    c = (68, Nothing, 10)
    d = (68, 20, Just 10)

functorTests :: TestTree
functorTests =
    let
        props :: [Test]
        props = unbatch $ functor xs
     in
        testGroup
            "Functor"
            [ QC.testProperties "Functor" props
            ]

applicativeTests :: TestTree
applicativeTests =
    let
        props :: [Test]
        props = unbatch $ applicative xs
     in
        testGroup
            "Applicative"
            [ QC.testProperties "Applicative" props
            ]
