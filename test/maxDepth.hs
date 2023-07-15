maxDepth :: Tree a -> Int
maxDepth (Leaf _) = 1
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)
