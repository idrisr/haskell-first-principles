module TestZip (qcProps) where

import qualified Zip as Z
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Data.List
import Data.Char

-- QuickCheck property
property :: (Eq a, Eq b) => [a] -> [b] -> Bool
property xs ys = Z.zip xs ys == zip xs ys

-- QuickCheck property
property2 :: Eq c => (a->b->c) -> [a] -> [b] -> Bool
property2 f xs ys = Z.zipWith f xs ys == zipWith f xs ys

qcProps = testGroup "(checked by QuickCheck)"
  [
  QC.testProperty "" $ \a b -> property (a::[Int]) (b::[Char]),
  QC.testProperty "" $ \a b -> property (a::[Integer]) (b::[Integer]),
  QC.testProperty "" $ \a b -> property (a::[Maybe Integer]) (b::[Either Bool Integer]),

  QC.testProperty "" $ \a b -> property2 (\c d -> ord d + c) (a::[Int]) (b::[Char]),
  QC.testProperty "" $ \a b -> property2 (*) (a::[Integer]) (b::[Integer]),
  QC.testProperty "" $ \a b -> property2 (\c d -> show c ++ show d) (a::[Maybe Integer]) (b::[Either Bool Integer])

  ]
