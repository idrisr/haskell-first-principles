module Three1Test (three1Tests) where

import Three1
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

three1Tests :: TestTree
three1Tests = testGroup "Three1" [functorTests, applicativeTests]

xs ::
    Three1
        (String, Sum Int, Maybe (Sum Int))
        (Sum Int, Product Int, Maybe (Sum Int))
xs = Three1 a b b
  where
    a = ("YO", 1, Nothing)
    b = (68, 20, Just 10)

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
