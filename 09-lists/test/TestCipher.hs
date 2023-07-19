module TestCipher (cipherProps) where

import Cipher
import Data.Char
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

{-
propertys
stays same case
shift 26 == identity
shift n  == shift (n `mod` 26)
-}

property :: Int -> Char -> Bool
property x c =
    isAsciiUpper c == isAsciiUpper (cipher x c)
        && isAsciiLower c == isAsciiLower (cipher x c)

property2 :: Char -> Bool
property2 c = cipher 26 c == c

property3 :: Int -> Char -> Bool
property3 x c = cipher x c == cipher (x `mod` 26) c

property4 :: Int -> Char -> Bool
property4 x c = (decipher x . cipher x) c == c

cipherProps =
    testGroup
        ""
        [ QC.testProperty "" $ \a b -> property (a :: Int) (b :: Char)
        , QC.testProperty "" $ \a -> property2 (a :: Char)
        , QC.testProperty "" $ \a b -> property3 (a :: Int) (b :: Char)
        , QC.testProperty "" $ \a b -> property4 (a :: Int) (b :: Char)
        ]
