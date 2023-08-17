module BiggerTests (biggerTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Bigger
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Bigger F1 F
xs = undefined

type F1 = ([String], Maybe Int, Product Int, Sum Int)
ys :: Bigger F F1
ys =  undefined

biggerTests :: TestTree
biggerTests = testGroup "Bigger" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
