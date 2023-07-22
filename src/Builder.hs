module Builder where
import Data.Binary.Builder (Builder)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Binary (Word64, Word8)
import Tree (Tree)
import View (Ptr (..))


data Serializer a = Serializer {
    builder:: Builder,
    size:: Word64
}

leafBuilder:: Word8 -> Serializer (Tree Word8)
leafBuilder v = Serializer (BB.word8 0x00 <> BB.word8 v) 2

nodeBuilder:: Word8 -> Serializer (Tree Word8) -> Serializer (Tree Word8)-> Serializer (Tree Word8)
nodeBuilder value left right =
    let leftBuilder = builder left
        leftSize = size left
        rightBuilder = builder right
        rightSize = size right
        rightOffset = BB.word64BE $ fromIntegral leftSize
    in Serializer (BB.word8 0x01 <> BB.word8 value <> rightOffset <> leftBuilder <> rightBuilder) (10 + leftSize + rightSize)

runSerializer:: Serializer a -> Ptr a
runSerializer value =
    let builderObject = builder value
        bs = B.toStrict $ BB.toLazyByteString builderObject
    in Ptr {buffer = bs, position = 0}
