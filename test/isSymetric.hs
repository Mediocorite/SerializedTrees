isSymmetric :: Eq a => Tree a -> Bool
isSymmetric tree = mirror tree tree
  where
    mirror (Leaf x) (Leaf y) = x == y
    mirror (Node x l1 r1) (Node y l2 r2) = x == y && mirror l1 r2 && mirror r1 l2
    mirror _ _ = False
