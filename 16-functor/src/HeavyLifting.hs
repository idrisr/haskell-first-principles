module HeavyLifting where

heavyA :: [Int]
heavyA = (+1) <$> read "[1]" :: [Int]

heavyB :: Maybe [String]
heavyB = fmap (++ "lol") <$> Just ["Hi,", "Hello"]

heavyC :: Integer -> Integer
heavyC = (*2) <$> (\x -> x - 2)

heavyD :: Int -> String
heavyD = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
