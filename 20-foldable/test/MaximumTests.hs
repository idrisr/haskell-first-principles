module MaximumTests (maxTests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified LibFuncs as LF
import Safe.Foldable

maxProp :: (Foldable t, Ord a) => t a -> Bool
maxProp xs = LF.maximum xs == maximumMay xs

maxTests :: TestTree
maxTests = testGroup "Max" [
    QC.testProperty "List" (maxProp::[Int] -> Bool)
  , QC.testProperty "()" (maxProp::(String, Int) -> Bool)
  , QC.testProperty "List Bool" (maxProp::[Bool] -> Bool)
    ]
