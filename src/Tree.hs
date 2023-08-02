{-# LANGUAGE InstanceSigs #-}
module Tree where

import System.Random (randomRIO)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)


calculateDepth :: Tree a -> Int
calculateDepth (Leaf _) = 1
calculateDepth (Node _ left right) = 1 + max (calculateDepth left) (calculateDepth right)

-- Generate a binary tree with a given depth and random values
generateTree :: Int -> Int -> IO (Tree Int, Int)
generateTree maxValue 0 = do
  value <- randomRIO (1, maxValue)
  return (Leaf value, 1)
generateTree maxValue depth = do
  leftDepth <- randomRIO (0, depth - 1)
  let rightDepth = depth - 1 - leftDepth
  (leftSubtree, leftTreeDepth) <- generateTree maxValue leftDepth
  (rightSubtree, rightTreeDepth) <- generateTree maxValue rightDepth
  value <- randomRIO (1, maxValue)
  return (Node value leftSubtree rightSubtree, 1 + max leftTreeDepth rightTreeDepth)

-- Generate 100 trees with values between 1 and 255 and depths from 2 to 100
generateTrees :: Int -> IO [(Tree Int, Int)]
generateTrees maxValue = mapM (generateTree maxValue) [2..100]

-- Convert a tree to a list of values
treeToList :: Tree Int -> [Int]
treeToList (Leaf value) = [value]
treeToList (Node value left right) = [value] ++ treeToList left ++ treeToList right

-- Convert a list of values to a tree
listToTree :: [Int] -> Tree Int
listToTree [] = error "Empty list cannot be converted to a tree."
listToTree [x] = Leaf x
listToTree (x:xs) = Node x (listToTree leftSubtree) (listToTree rightSubtree)
  where
    (leftSubtree, rightSubtree) = splitList xs
    splitList :: [a] -> ([a], [a])
    splitList lst = splitAt (length lst `div` 2) lst
