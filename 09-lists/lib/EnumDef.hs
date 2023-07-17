module EnumDef where

eftBool :: Bool -> Bool -> [Bool]
eftBool  = enumFromTo

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = enumFromTo

eftInt :: Int -> Int -> [Int]
eftInt = enumFromTo

eftChar :: Char -> Char -> [Char]
eftChar = enumFromTo
