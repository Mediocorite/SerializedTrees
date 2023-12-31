module PathSum where

import Data.Word (Word8)
import Tree (Tree(..))
import View (Ptr(..), view, View(..))
import Deserializer (deserialize)

-- For Serialized Trees
pathSum :: Ptr (Tree Word8) -> Word8 -> Bool
pathSum ptr targetSum = pathSumHelper ptr 0 targetSum

pathSumHelper :: Ptr (Tree Word8) -> Word8 -> Word8 -> Bool
pathSumHelper ptr currentSum targetSum =
    case view ptr of
        VLeaf v -> currentSum + v == targetSum
        VNode v left right ->
            let newSum = currentSum + v
            in pathSumHelper left newSum targetSum || pathSumHelper right newSum targetSum

-- For Traditional Trees
traditionalPathSum :: Tree Int -> Int -> Bool
traditionalPathSum tree = traditionalPathSumHelper tree 0

traditionalPathSumHelper :: Tree Int -> Int -> Int -> Bool
traditionalPathSumHelper (Leaf v) currentSum targetSum = currentSum + v == targetSum
traditionalPathSumHelper (Node v left right) currentSum targetSum =
    let newSum = currentSum + v
    in traditionalPathSumHelper left newSum targetSum || traditionalPathSumHelper right newSum targetSum

--For Deserialize and PathSum
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)
deserializePathSum :: Ptr (Tree Word8) -> Int -> Bool
deserializePathSum = traditionalPathSum . mapTree fromIntegral . deserialize