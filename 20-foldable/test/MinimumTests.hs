module MinimumTests (minTests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified LibFuncs as LF
import Safe.Foldable

minProp :: (Foldable t, Ord a) => t a -> Bool
minProp xs = LF.minimum xs == minimumMay xs

minTests :: TestTree
minTests = testGroup "Min" [
    QC.testProperty "List" (minProp::[Int] -> Bool)
  , QC.testProperty "()" (minProp::(String, Int) -> Bool)
  , QC.testProperty "List Bool" (minProp::[Bool] -> Bool)
    ]
