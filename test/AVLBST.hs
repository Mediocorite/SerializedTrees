module AVLBST where

import Tree (Tree(..))

height :: Tree a -> Int
height (Leaf _) = 0
height (Node _ left right) = 1 + max (height left) (height right)

balanceFactor :: Tree a -> Int
balanceFactor (Leaf _) = 0
balanceFactor (Node _ left right) = height right - height left

rotateLeft :: Tree a -> Tree a
rotateLeft (Node x left (Node y rightM rightN)) =
    Node y (Node x left rightM) rightN
rotateLeft _ = error "Invalid rotation"

rotateRight :: Tree a -> Tree a
rotateRight (Node x (Node y leftM leftN) right) =
    Node y leftM (Node x leftN right)
rotateRight _ = error "Invalid rotation"

balance :: Tree a -> Tree a
balance tree
    | bf > 1 && balanceFactor right < 0 = rotateLeft (Node x left (rotateRight right))
    | bf > 1 = rotateLeft tree
    | bf < -1 && balanceFactor left > 0 = rotateRight (Node x (rotateLeft left) right)
    | bf < -1 = rotateRight tree
    | otherwise = tree
  where
    bf = balanceFactor tree
    (Node x left right) = tree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x (Leaf val)
    | x < val = Node val (Leaf x) (Leaf val)
    | otherwise = Node val (Leaf val) (Leaf x)
insert x (Node val left right)
    | x < val = balance (Node val (insert x left) right)
    | x > val = balance (Node val left (insert x right))
    | otherwise = Node val left right

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert (Leaf (error "Empty tree"))
