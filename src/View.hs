module View where

import qualified Data.ByteString as B
import Data.Binary (Word8)
import Tree ( Tree )

-- This creates a single level 

data Ptr t = Ptr {
    buffer :: B.ByteString,
    position :: Int
}

data View a 
    = VLeaf a
    | VNode a (Ptr (Tree a)) (Ptr (Tree a))
    
-- view:: Ptr (Tree Word8) -> View Word8