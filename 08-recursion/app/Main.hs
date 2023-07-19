module Main where

import Division

-- 15 / 4 0
-- 11 / 4 1
-- 7  / 4 2
-- 3  / 4 3

main :: IO ()
main = print $ 15 `dividedBy` (4 :: Int)
