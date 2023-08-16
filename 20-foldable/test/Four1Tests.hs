module Four1Tests (four1Tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Four1
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
type G = (String, Product Int, Sum Int, Product Integer, Sum Int)

xs :: Four1 F G
xs = let
    a :: F
    a = (69, "YO", 420, 10, 5150)
    b :: G
    b = ("YO", 420, 10, 5150, 69)
    in Four1 a b b b

four1Tests :: TestTree
four1Tests = testGroup "Four1" [foldableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]
