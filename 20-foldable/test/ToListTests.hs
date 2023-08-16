module ToListTests (toListTests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified LibFuncs as LF
import Data.Foldable

toListProp :: (Foldable t, Eq a) => t a -> Bool
toListProp xs = LF.toList xs == toList xs

type PropMaybe = Maybe Int -> Bool
type PropTup = (Int, String) -> Bool
type PropEither = Either Double Char -> Bool

toListTests :: TestTree
toListTests = testGroup "ToList" [
    QC.testProperty "List" (toListProp::PropMaybe)
  , QC.testProperty "()" (toListProp::PropTup)
  , QC.testProperty "List Bool" (toListProp::PropEither)
    ]
