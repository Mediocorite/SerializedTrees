module Traversal (traverseRight, traverseLeft) where

import qualified Data.ByteString as B
import Data.Word (Word8)
import Serialization (bsToIntList)
import View (Ptr(..), View (..), view)
import Tree(Tree)

-- traverseRight :: B.ByteString -> Int -> [Word8]
-- traverseRight bs offset
--   | offset >= B.length bs = []
--   | otherwise =
--     case B.index bs offset of
--       0x01 ->
--         let val = fromIntegral (B.index bs (offset + 1))
--             off = fromIntegral (B.index bs (offset + 2))
--         in val : traverseRight (B.drop (offset + 3) bs) off
--       0x00 -> [fromIntegral (B.index bs (offset + 1))]
--       _ -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)

traverseRight:: Ptr (Tree Word8) -> [Word8]
traverseRight pointer = case view pointer of 
    VLeaf value -> [value]
    VNode value leftPointer rightPointer -> value : traverseRight rightPointer


traverseLeft:: Ptr (Tree Word8) -> [Word8]
traverseLeft pointer = case view pointer of 
    VLeaf value -> [value]
    VNode value leftPointer rightPointer -> value : traverseLeft leftPointer