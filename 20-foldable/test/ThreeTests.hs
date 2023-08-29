module ThreeTests (threeTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Three
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
type G = (String, Product Int, Sum Int, Product Integer, Sum Int)
type H = (Product Int, Sum Int, Product Integer, Sum Int, String)

xs :: Three F G H
xs = undefined

threeTests :: TestTree
threeTests = testGroup "Three" [foldableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]
