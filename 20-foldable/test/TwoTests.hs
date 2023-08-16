module TwoTests (twoTests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Two
import Data.Monoid

type F = (Sum Int, String, Product Int, Sum Int, Product Integer)
type G = (String, Product Int, Sum Int, Product Integer, Sum Int)

xs :: Two F G
xs = let
    a :: F
    a = (69, "YO", 420, 10, 5150)
    b :: G
    b = ("YO", 420, 10, 5150, 69)
    in Two a b

twoTests :: TestTree
twoTests = testGroup "Two" [foldableTests]

foldableTests :: TestTree
foldableTests =
    let props = unbatch $ foldable xs
     in testGroup "Foldable" [ testProperties "" props ]
