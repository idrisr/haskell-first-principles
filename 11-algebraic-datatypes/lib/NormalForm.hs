module NormalForm where

type AuthorName = String
data Fiction0 = Fiction0 deriving Show
data NonFiction0 = NonFiction0 deriving Show

data BookType = FictionBook Fiction0
            | NonFictionBook NonFiction0
            deriving Show

-- product of sums
-- (a + b) * (c)
{-# ANN Author0 "HLint: Use newtype instead of data" #-}
data Author0 = Author0 (AuthorName, BookType)

-- sum of products
-- (a * c) + (b * c)
data Author =
    Fiction AuthorName
    | NonFiction AuthorName
    deriving (Eq, Show)

data Expr =
    Number Int
    | Add Expr Expr
    | Minus Expr
    | Mult Expr Expr
    | Divide Expr Expr

data FlowerType =
    Gardenia
    | Daisy
    | Rose
    | Lilac
    deriving Show

type Gardener = String

data Garden =
    Garden Gardener FlowerType
    deriving Show

data Garden0 =
    Gardenia0 Gardener
    | Daisy0 Gardener
    | Rose0 Gardener
    | Lilac0 Gardener
