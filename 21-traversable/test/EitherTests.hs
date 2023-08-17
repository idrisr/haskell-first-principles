module EitherTests (eitherTests) where

import Prelude hiding (Either, Left, Right)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Either
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
type G = ([String], Sum Int, String, Product Int, Sum Int)

xs :: Either F G
xs = undefined

type F1 = ([String], Maybe Int, Product Int, Sum Int)
type G1 = (Maybe Int, [String], Product Int, Sum Int)
ys :: Either F1 G1
ys = undefined

eitherTests :: TestTree
eitherTests = testGroup "Either" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
