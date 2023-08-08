module Deserializer where

import Tree (Tree(..))
import View (Ptr(..), view, View(..))
import Data.Word (Word8)

deserialize :: Ptr (Tree Word8) -> Tree Word8
deserialize pointer = 
    case view pointer of
        VLeaf v -> Leaf v
        VNode v left right -> Node v (deserialize left) (deserialize right)
