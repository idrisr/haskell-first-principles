module Database where

import Data.Time

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 9002
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

-- 1. Write a function that filters for DbDate values and returns a list of the
-- UTCTime values inside them:
--

isDate :: DatabaseItem -> Bool
isDate (DbString _) = False
isDate (DbNumber _) = False
isDate (DbDate _) = True

getDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
getDate (DbString _) xs = xs
getDate (DbNumber _) xs = xs
getDate (DbDate x) xs = x : xs

getNumber :: DatabaseItem -> [Integer] -> [Integer]
getNumber (DbString _) xs = xs
getNumber (DbNumber x) xs = x:xs
getNumber (DbDate _) xs = xs

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDate []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumber []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = maximum $ filterDbDate xs

sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum $ filterDbNumber xs

avgDb :: [DatabaseItem] -> Double
avgDb xs = let ys = filterDbNumber xs in
    fromIntegral (sum ys) / fromIntegral (length ys)
