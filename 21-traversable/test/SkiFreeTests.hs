module SkiFreeTests (skiFreeTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import SkiFree
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: S Maybe F
xs = undefined

type F1 = ([String], Maybe Int, Product Int, Sum Int)
ys :: S [] F1
ys =  undefined

skiFreeTests :: TestTree
skiFreeTests =
    testGroup
        "SkiFree"
        [ foldableTests
        , traversableTests
        ]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
