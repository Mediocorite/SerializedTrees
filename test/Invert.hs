module Invert where
    
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as BB
-- import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

import Tree (Tree(..))
import View (Ptr(..), 
            view, 
            -- int64Value, 
            View(..))
import Builder (Serializer, leafBuilder, nodeBuilder)

-- invert :: Ptr (Tree Word8) -> B.ByteString
-- invert ptr = case view ptr of
--     VLeaf v -> B.pack [0x00, v]  -- Serialize a leaf node directly
--     VNode v leftPtr rightPtr ->
--         let invertedLeft = invert (Ptr (B.drop 10 (buffer leftPtr)) 0)  -- Drop the first 10 bytes to skip the node header
--             invertedRight = invert (Ptr (B.drop (10 + int64Value (B.take 8 (B.drop 2 (buffer rightPtr)))) (buffer rightPtr)) 0) -- Drop the first 10 + offset bytes
--             invertedOffset = BB.toLazyByteString (BB.word64BE (fromIntegral (B.length invertedRight)))
--         in B.concat [B.pack [0x01, v], BSL.toStrict invertedOffset, invertedRight, invertedLeft]

invert:: Ptr (Tree Word8) -> Serializer (Tree Word8)
invert pointer = 
    case view pointer of
        VLeaf v -> leafBuilder v
        VNode v left right -> nodeBuilder v (invert right) (invert left)

traditionalInvertTree :: Tree a -> Tree a
traditionalInvertTree (Leaf val) = Leaf val
traditionalInvertTree (Node val left right) = Node val (traditionalInvertTree right) (traditionalInvertTree left)
