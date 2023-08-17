module BabyStep where

data Record

myData :: [String]
myData = undefined

myFunc :: String -> IO Record
myFunc = undefined

wrong :: [IO Record]
wrong = fmap myFunc myData

right :: IO [Record]
right = traverse myFunc myData
