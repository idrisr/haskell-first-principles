module BadMonoidTest where

import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Checkers
import BadMonoid

props :: [Test]
props = unbatch $ monoid Twoo

xs :: [(String, String, Integer)]
xs = [("b", "w", 1)]

props2 :: [Test]
props2 = unbatch $ applicative xs

monoidTests :: TestTree
monoidTests = QC.testProperties "Monoid" props

appTests :: TestTree
appTests = QC.testProperties "Applicative" props2
