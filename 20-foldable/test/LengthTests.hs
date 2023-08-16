module LengthTests (lengthTests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified LibFuncs as LF

lengthProp :: Foldable t => t a -> Bool
lengthProp xs = LF.length xs == length xs

lengthTests :: TestTree
lengthTests = testGroup "Length" [
    QC.testProperty "List" (lengthProp::[Int] -> Bool)
  , QC.testProperty "()" (lengthProp::(String, Int) -> Bool)
  , QC.testProperty "List Bool" (lengthProp::[Bool] -> Bool)
    ]
