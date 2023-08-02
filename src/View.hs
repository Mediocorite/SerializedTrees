{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module View where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get (getWord64be, runGet)
import Data.Binary (Word8)
import Tree ( Tree )
import Serialization (bsToIntList)

data Ptr t = Ptr {
    buffer :: B.ByteString,
    position :: Int
}

instance Show (Ptr (Tree Word8)) where
  show :: Ptr (Tree Word8) -> String
  show ptr = "Ptr { buffer = " ++ show (buffer ptr) ++ ", position = " ++ show (position ptr) ++ " }"

instance Eq (Ptr (Tree Word8)) where
  (==) :: Ptr (Tree Word8) -> Ptr (Tree Word8) -> Bool
  (Ptr buffer1 position1) == (Ptr buffer2 position2) = buffer1 == buffer2 && position1 == position2


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
        0x00 -> 
            --           ------------------
            --  content: | tag   | val    | 
            --  size:    | 1     | 1      | 
            --  address: | pos   | pos+1  | 
            --           ------------------
            VLeaf (B.index bs (pos + 1))
        0x01 -> 
            --  This is the breakdown for first
            --           --------------------------------------------------------
            --  content: | tag   | val    | offset  | left tree | right tree    |
            --  size:    | 1     | 1      | 8       | offset    | ?             |
            --  address: | pos   | pos+1  | pos+2   | pos+10    | pos+10+offset |
            --           --------------------------------------------------------
            let val         = B.index bs (pos + 1)
                -- Get the offset value (8 bytes)
                offset      = int64Value (B.take 8 (B.drop (2 + pos) bs))
                leftPtr     = Ptr { buffer=bs, position = pos + 10} -- Add the size of node
                rightPtr    = Ptr { buffer=bs, position = pos + 10 + offset}
            in VNode val leftPtr rightPtr
        __  -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)
