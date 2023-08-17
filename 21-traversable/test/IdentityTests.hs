module IdentityTests (identityTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Identity
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
xs :: Identity F
xs = undefined

type G = ([String], Maybe Int, Product Int, Sum Int)
ys :: Identity G
ys = undefined

identityTests :: TestTree
identityTests = testGroup "Identity" [foldableTests, traversableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]

traversableTests :: TestTree
traversableTests =
    let props = unbatch $ traversable ys
     in testGroup "Traversable" [ testProperties "" props ]
