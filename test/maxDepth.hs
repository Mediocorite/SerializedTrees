-- Find the maximum depth of the binary tree using the pointer
module MaxDepth where
    
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as BB
-- import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

import Tree (Tree(..))
import View (Ptr(..), view, int64Value, View(..))
maxDepth :: Ptr (Tree Word8) -> Int
maxDepth ptr = case view ptr of
    VLeaf _ -> 1
    VNode _ leftPtr rightPtr -> 1 + max (maxDepth (Ptr (B.drop 10 (buffer leftPtr)) 0)) (maxDepth (Ptr (B.drop 10 (buffer rightPtr)) 0))