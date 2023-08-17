module ThreeTests (threeTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Three
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Three F F1 F
xs = undefined

type F1 = ([String], Maybe Int, Product Int, Sum Int)
ys :: Three F1 F1 F1
ys =  undefined

threeTests :: TestTree
threeTests = testGroup "Three" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
