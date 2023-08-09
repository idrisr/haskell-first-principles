module WarmupAndReview where

stops :: String
stops = "pbtdkg"
vowels :: String
vowels = "aeiou"

combos :: String -> String -> [(Char, Char, Char)]
combos xs ys = [(a, b, c) | a <-xs, b <- ys, c<-xs]

combosP :: [(Char, Char, Char)] -> [(Char, Char, Char)]
combosP = filter (\(a, _, _) -> a == 'p')

seekritFunc :: String -> Float
seekritFunc x = fromIntegral a / fromIntegral b
    where a = sum $ map length $ words x
          b = length $ words x
