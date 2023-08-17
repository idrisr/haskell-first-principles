module ListTests (listTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import List
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: List F
xs = undefined

type F1 = ([String], Maybe Int, Product Int, Sum Int)
ys :: List F1
ys =  undefined

listTests :: TestTree
listTests = testGroup "List" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
