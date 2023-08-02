-- Calculate the path sum of the binary tree using the pointer
module PathSum where
  
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as BB
-- import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

import Tree (Tree(..))
import View (Ptr(..), view, 
                -- int64Value, 
                View(..)
                )
-- import Serialization (serializeTree)
pathSum :: Ptr (Tree Word8) -> Int
pathSum ptr = case view ptr of
    VLeaf v -> fromIntegral v
    VNode v leftPtr rightPtr -> fromIntegral v + pathSum (Ptr (B.drop 10 (buffer leftPtr)) 0) + pathSum (Ptr (B.drop 10 (buffer rightPtr)) 0)