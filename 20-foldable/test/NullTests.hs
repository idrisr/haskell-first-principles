module NullTests (nullTests) where

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified LibFuncs as LF

nullProp :: Foldable t => t a -> Bool
nullProp xs = LF.null xs == null xs

nullTests :: TestTree
nullTests = testGroup "Null" [
    QC.testProperty "List" (nullProp::[Int] -> Bool)
  , QC.testProperty "()" (nullProp::(String, Int) -> Bool)
  , QC.testProperty "List Bool" (nullProp::[Bool] -> Bool)
    ]
