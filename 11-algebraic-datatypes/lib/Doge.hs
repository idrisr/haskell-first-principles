module Doge where

data PugType = PugData
data HuskyType a = HuskyData
{- hlint ignore "Use newtype instead of data" -}
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug :: PugType
myPug = PugData

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a
    = Husky a
    | Mastiff a
    deriving (Eq, Show)
