module Comprehension where

mySqr :: [Integer]
mySqr = let p=(2::Integer) in [x^p | x <- [1..10]]

a :: [Integer]
a = [x | x <- mySqr, rem x 2 == 10]

b :: [(Integer, Integer)]
b = [(x, y) |
        x <- mySqr,
        x < 50,
        y <- mySqr,
        y > 50
        ]

c :: [(Integer, Integer)]
c = take 5 [ (x, y) |
                x <- mySqr,
                x < 50,
                y <- mySqr,
                y > 50
                ]
