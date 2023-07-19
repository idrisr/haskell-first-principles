module Cipher (decipher, cipher) where

import Data.Char

shift :: Int -> Char -> Char -> Char
shift x y c = chr $ (ord c - b + x) `mod` 26 + b
    where b = ord y

cipher :: Int -> Char -> Char
cipher x c | isAsciiLower c = shift x 'a' c
cipher x c | isAsciiUpper c = shift x 'A' c
cipher _ c = c

decipher :: Int -> Char -> Char
decipher x c | isAsciiLower c = shift (-x) 'a' c
decipher x c | isAsciiUpper c = shift (-x) 'A' c
decipher _ c = c
