module Main where

import Test.QuickCheck

main :: IO ()
main = do
    sample (arbitrary :: Gen Int)
    sample (arbitrary :: Gen Double)
