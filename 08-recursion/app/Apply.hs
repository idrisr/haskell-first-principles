module Main where

incTimes :: (Num b, Eq a, Num a) => a -> b -> b
incTimes 0 b = b
incTimes n b = 1 + incTimes (n-1) b

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f $ applyTimes (n-1) f b

incTimes' :: (Num b, Eq a, Num a) => a -> b -> b
incTimes' = flip applyTimes (+1)

main :: IO ()
main = do
    let a = incTimes (10::Int) 59::Int
    let b = applyTimes (10::Int) (+1) 10::Int
    let c = incTimes' (10::Int) 59::Int
    print a
    print b
    print c
