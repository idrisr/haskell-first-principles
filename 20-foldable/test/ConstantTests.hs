module ConstantTests (constantTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Constant
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)

xs :: Constant F F
xs = Constant (69, "YO", 420, 10, 5150)

constantTests :: TestTree
constantTests = testGroup "Constant" [foldableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]
