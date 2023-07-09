module View where

import qualified Data.ByteString as B
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
            let val         = B.index bs (pos + 1)
                leftOffset  = fromIntegral (B.index bs (pos + 2))
                rightOffset = fromIntegral (B.index bs (pos + 3))
                leftPtr     = Ptr bs (pos + 3 + leftOffset)
                rightPtr    = Ptr bs (pos + 3 + rightOffset)
            in VNode val leftPtr rightPtr
        _    -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)

