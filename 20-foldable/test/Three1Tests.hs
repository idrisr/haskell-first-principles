module Three1Tests (three1Tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Three1
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
type G = (String, Product Int, Sum Int, Product Integer, Sum Int)

xs :: Three1 F G
xs = let
    a :: F
    a = (69, "YO", 420, 10, 5150)
    b :: G
    b = ("YO", 420, 10, 5150, 69)
    in Three1 a b b

three1Tests :: TestTree
three1Tests = testGroup "Three1" [foldableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]
