module ConstantTests (constantTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Constant
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Constant G F
xs = undefined

type G = (Maybe Integer, Maybe Int, Product Int, Sum Int)
ys :: Constant F G
ys = undefined

constantTests :: TestTree
constantTests = testGroup "Constant" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
