module View where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get (getWord64be, runGet)
import Data.Binary (Word8)
import Tree ( Tree )
import Serialization (bsToIntList)
import Debug.Trace (trace)


data Ptr t = Ptr {
    buffer :: B.ByteString,
    position :: Int
}

data View a = VLeaf a | VNode a (Ptr (Tree a)) (Ptr (Tree a))

-- I am still trying to find if there is a way to achieve this
-- without converting into a lazy Byte string. Given it's only 
-- doing so with 8 bytes, i think it can be ignored for now
int64Value :: B.ByteString -> Int
int64Value bs = fromIntegral (runGet getWord64be $ BSL.fromStrict bs) :: Int

view:: Ptr (Tree Word8) -> View Word8
view pointer = 
    let bs  = buffer pointer
        pos = position pointer
    in case B.index bs pos of
        0x00 -> VLeaf (B.index bs (pos + 1))
        0x01 ->
            let val         = B.index bs (pos + 1)
                -- Get the offset value (8 bytes)
                offset      = int64Value (B.take 8 (B.drop (2 + pos) bs))
                leftPtr     = Ptr { buffer=bs, position = pos + 10} -- Add the size of node
                rightPtr    = Ptr { buffer=bs, position = pos + 10 + offset}
            in VNode val leftPtr rightPtr
        __  -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)

-- view :: Ptr (Tree Word8) -> View Word8
-- view pointer = 
--     let bs  = buffer pointer
--         pos = position pointer
--     in case B.index bs pos of
--         0x00 -> do
--             let val = B.index bs (pos + 1)
--             trace ("Stage 1: Found leaf node with value " ++ show val) (VLeaf val)
--         0x01 -> do
--             let val = B.index bs (pos + 1)
--                 offset = int64Value (B.take 8 (B.drop 2 bs))
--                 leftPtr = Ptr { buffer=bs, position = pos + 11 } -- Add the size of node
--                 rightPtr = Ptr { buffer=bs, position = pos + 10 + 1 }
--             trace ("Stage 2: Found internal node with value " ++ show val ++ " and offset " ++ show offset) (VNode val leftPtr rightPtr)
-- --         _ -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)
