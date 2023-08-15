module Four1Test (four1Tests) where

import Four1
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import Data.Monoid

four1Tests :: TestTree
four1Tests = testGroup "Four1" [functorTests, applicativeTests]

xs ::
    Four1
        (Sum Int, Product Int, Maybe (Sum Int))
        (Maybe (Sum Int), Sum Int, Product Int)
xs = Four1 a a a b
  where
    a = (420, 1, Nothing)
    b = (Just 68, 20, 11)

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
