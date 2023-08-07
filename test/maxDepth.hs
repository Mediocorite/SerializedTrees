-- Find the maximum depth of the binary tree using the pointer
module MaxDepth where
    
import Data.Word (Word8)
import Tree (Tree(..))
import View (Ptr(..), view, View(..))

-- For Serialized Trees
maxDepth :: Ptr (Tree Word8) -> Int
maxDepth ptr = 
    case view ptr of
        VLeaf _ -> 1
        VNode _ left right -> 1 + max (maxDepth left) (maxDepth right)

-- For Traditional Trees
traditionalMaxDepth :: Tree a -> Int
traditionalMaxDepth (Leaf _) = 1
traditionalMaxDepth (Node _ left right) = 1 + max (traditionalMaxDepth left) (traditionalMaxDepth right)
