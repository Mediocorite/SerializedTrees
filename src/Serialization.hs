module Serialization where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Base16 (encode)
import Data.Word (Word8, Word64)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Tree (Tree(..))

serialize :: Tree Word8 -> B.ByteString
serialize tree = B.toStrict $ BB.toLazyByteString $ fst $ serializeTree tree

serializeTree :: Tree Word8 -> (BB.Builder, Word64)
serializeTree (Leaf value) = (BB.word8 0x00 <> BB.word8 value, 2)
serializeTree (Node value left right) =
  let (leftSerialized, sizeLeft) = serializeTree left
      (rightSerialized, sizeRight) = serializeTree right
      rightOffset = BB.word64BE $ fromIntegral sizeLeft -- size of 8 bytes
  -- Size of 10 for Node | Offset | Value -> 1 Byte | 1 Byte | 8 Bytes
  in (BB.word8 0x01 <> BB.word8 value <> rightOffset <> leftSerialized <> rightSerialized, 10 + sizeLeft + sizeRight)

printByteStringAsHex :: B.ByteString -> IO ()
printByteStringAsHex bs = putStrLn $ unwords $ chunksOf 2 $ show $ encode bs

printByteStringAsInt :: B.ByteString -> IO ()
printByteStringAsInt bs = putStrLn $ unwords $ map show $ B.unpack bs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

bsToIntList :: B.ByteString -> [Int]
bsToIntList bs = map ord (unpack bs)
