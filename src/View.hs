module View where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Binary (Word8)
import Tree ( Tree )
import Serialization (bsToIntList)

data Ptr t = Ptr {
    buffer :: B.ByteString,
    position :: Int
}

data View a = VLeaf a | VNode a (Ptr (Tree a)) (Ptr (Tree a))

view:: Ptr (Tree Word8) -> View Word8
view pointer = 
    let bs  = buffer pointer
        pos = position pointer
    in case B.index bs pos of
        0x00 -> VLeaf (B.index bs (pos + 1))
        0x01 -> 
            let val             = B.index bs (pos + 1)
                -- Get the offset value (8 bytes)
                offset          = fromIntegral (B.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 (B.take 8 (B.drop 2 bs)))
                leftPtr         = Ptr { buffer=bs, position =  pos + 10}
                rightPtr        = Ptr { buffer=bs, position = offset }
            in VNode val leftPtr rightPtr
        _    -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)

