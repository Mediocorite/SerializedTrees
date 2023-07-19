-- Level order traversal using the pointer
module LevelOrder where 
  
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

import Tree (Tree(..))
import View (Ptr(..), view, int64Value, View(..))
levelOrder :: Ptr (Tree Word8) -> [Word8]
levelOrder ptr = levelOrderHelper [ptr]

levelOrderHelper :: [Ptr (Tree Word8)] -> [Word8]
levelOrderHelper [] = []
levelOrderHelper (currentPtr : rest) = case view currentPtr of
    VLeaf v -> v : levelOrderHelper rest
    VNode v leftPtr rightPtr -> v : levelOrderHelper (rest ++ [leftPtr, rightPtr])