module McCarthy91 where

mccarthy91 :: Integral a => a -> a
mccarthy91 n | n > 100 = n - 10
mccarthy91 n  = mccarthy91 . mccarthy91 $ n + 11
