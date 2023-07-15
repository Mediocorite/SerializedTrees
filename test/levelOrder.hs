levelOrder :: Tree a -> [[a]]
levelOrder tree = traverseLevels [tree]
  where
    traverseLevels [] = []
    traverseLevels nodes =
      let values = map getValue nodes
          children = concatMap getChildren nodes
      in values : traverseLevels children

    getValue (Leaf x) = x
    getValue (Node x _ _) = x

    getChildren (Leaf _) = []
    getChildren (Node _ left right) = [left, right]
