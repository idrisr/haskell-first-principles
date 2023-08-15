module Doing where

import Control.Monad (join)

h :: String
h = "Hello, "
w :: String
w = "World, "

a :: IO ()
a = putStrLn h >> putStrLn w

a1 :: IO ()
a1 = putStrLn h *> putStrLn w

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing1 :: IO ()
sequencing1 =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing2 :: IO ()
sequencing2 = putStrLn "blah" *> putStrLn "another thing"

sequencing3 :: IO ()
sequencing3 = putStrLn "blah" >>= const (putStrLn "another thing")

b :: IO ()
{- hlint ignore "Use =<<" -}
b = join $ putStrLn <$> getLine

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x then [x, x] else []
