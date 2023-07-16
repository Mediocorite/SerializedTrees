module Tree where

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
