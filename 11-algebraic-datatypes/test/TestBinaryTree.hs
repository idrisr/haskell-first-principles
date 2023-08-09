module TestBinaryTree where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import BinaryTree

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

binaryTreeTests = testGroup "binarytree"
  [
    let s = Node (Node Leaf (3::Integer) Leaf) 1 (Node Leaf 4 Leaf)
        ans = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf) in
  testCase "map" $ mapTree (+1) s `compare` ans @?= EQ

  , let ans = [2, 1, 3] in
      testCase "preorder" $ preorder testTree `compare` ans @?= EQ

  , let ans = [1, 2, 3] in
      testCase "inorder" $ inorder testTree `compare` ans @?= EQ

  , let ans = [1, 3, 2] in
      testCase "postorder" $ postorder testTree `compare` ans @?= EQ
  ]
