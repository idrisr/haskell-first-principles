module SquareCube where

mySqr :: [Integer]
mySqr = [x^(2::Integer) | x <- [(1::Integer)..5]]

myCube :: [Integer]
myCube = [y^(3::Integer) | y <- [(1::Integer)..5]]

a = [(x, y) | x <- mySqr, y <- myCube]
b = [(x, y) |
    x <- mySqr,
    x < 50,
    y <- myCube,
    y < 50]
c = length b
