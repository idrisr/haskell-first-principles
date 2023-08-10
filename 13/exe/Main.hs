module Main where

import Hello
import DogsRule
import System.IO

main :: IO ()
main =
    -- hSetBuffering stdout NoBuffering >>
    putStr "Please input your name: " >>
    getLine >>=
    \name -> sayHello name
    >> dogs
