module OptionalTests (optionalTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Optional
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Optional F
xs = undefined

type G = ([String], Maybe Int, Product Int, Sum Int)
ys :: Optional G
ys = undefined

optionalTests :: TestTree
optionalTests = testGroup "Optional" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
