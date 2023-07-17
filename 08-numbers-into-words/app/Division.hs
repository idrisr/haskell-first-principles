module Main where

data DividedResult = Result Integer | DivideByZero
    deriving (Show)

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy _ 0 = DivideByZero
dividedBy n d
    | n > 0 && d < 0 =
        let a = dividedBy n (-d)
         in case a of
                (Result x) -> Result (-x)
                DivideByZero -> DivideByZero
dividedBy n d
    | n < 0 && d > 0 =
        let a = dividedBy (-n) d
         in case a of
                (Result x) -> Result (-x)
                DivideByZero -> DivideByZero
dividedBy n d | n < 0 && d < 0 = dividedBy (-n) (-d)
dividedBy n d = Result $ go n d 0
  where
    go a b c
        | a < b = c
        | otherwise = go (a - b) b c + 1

-- 15 / 4 0
-- 11 / 4 1
-- 7  / 4 2
-- 3  / 4 3

main :: IO ()
main = print $ 15 `dividedBy` (4 :: Int)
