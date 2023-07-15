hasPathSum :: Tree Int -> Int -> Bool
hasPathSum (Leaf val) targetSum = val == targetSum
hasPathSum (Node val left right) targetSum =
  hasPathSum left (targetSum - val) || hasPathSum right (targetSum - val)
