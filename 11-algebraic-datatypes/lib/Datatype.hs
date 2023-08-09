module Datatype where

import Newtype
import Algebra
import NormalForm
import ConDeStruct
import BinaryTree

data Manufacturer = Mini | Mazda | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

newtype Price = Price Integer
    deriving (Eq, Show)

data Size = Size
    deriving (Show, Eq)

data Vehicle = Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _ _) = False

isPlane :: Vehicle -> Bool
isPlane (Car _ _) = False
isPlane (Plane _ _) = True

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu (Plane _ _) = Nothing

asdf = tooManyGoats $ Goats 31
