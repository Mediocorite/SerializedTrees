module Traversal where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Serialization (bsToIntList)
import View (Ptr(..), View (..), view)
import Tree(Tree)

traverseRight:: Ptr (Tree Word8) -> [Word8]
traverseRight pointer = case view pointer of 
    VLeaf value -> [value]
    VNode value leftPointer rightPointer -> value : traverseRight rightPointer


traverseLeft:: Ptr (Tree Word8) -> [Word8]
traverseLeft pointer = case view pointer of 
    VLeaf value -> [value]
    VNode value leftPointer rightPointer -> value : traverseLeft leftPointer