invertTree :: Tree a -> Tree a
invertTree (Leaf x) = Leaf x
invertTree (Node x left right) = Node x (invertTree right) (invertTree left)
