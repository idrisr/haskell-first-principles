module BinaryTree where

data BinaryTree a = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Eq, Ord)

mapTree :: (a-> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node l a r) = Node l1 (f a) r1
    where l1 = mapTree f l
          r1 = mapTree f r

-- read left right
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : preorder l ++ preorder r

-- left read right
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = inorder l ++ [a] ++ inorder r

-- left right read
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b t = foldr f b $ inorder t
