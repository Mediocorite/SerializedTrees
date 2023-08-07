module LevelOrder where
    
import Data.Word (Word8)
import Tree (Tree(..))
import View (Ptr(..), view, View(..))

-- For Serialized Trees
levelOrder :: Ptr (Tree Word8) -> [[Word8]]
levelOrder ptr = levelOrderHelper [ptr]

levelOrderHelper :: [Ptr (Tree Word8)] -> [[Word8]]
levelOrderHelper [] = []
levelOrderHelper pointers =
    let (values, nextPtrs) = unzip $ map fetchValue pointers
    in values : levelOrderHelper (concat nextPtrs)

fetchValue :: Ptr (Tree Word8) -> (Word8, [Ptr (Tree Word8)])
fetchValue ptr = 
    case view ptr of
        VLeaf v -> (v, [])
        VNode v left right -> (v, [left, right])

-- For Traditional Trees
traditionalLevelOrder :: Tree a -> [[a]]
traditionalLevelOrder tree = traditionalLevelOrderHelper [tree]

traditionalLevelOrderHelper :: [Tree a] -> [[a]]
traditionalLevelOrderHelper [] = []
traditionalLevelOrderHelper trees =
    let (values, nextTrees) = unzip $ map fetchValueTraditional trees
    in values : traditionalLevelOrderHelper (concat nextTrees)

fetchValueTraditional :: Tree a -> (a, [Tree a])
fetchValueTraditional (Leaf v) = (v, [])
fetchValueTraditional (Node v left right) = (v, [left, right])
