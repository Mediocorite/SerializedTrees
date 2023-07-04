module Serialization (serialize, printByteStringAsInt, printByteStringAsHex, bsToIntList) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base16 (encode)
import Data.Word (Word8)
import Data.Int (Int64)
import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Tree (Tree(..))

serialize :: Tree Word8 -> B.ByteString
serialize tree = B.toStrict $ BB.toLazyByteString $ serializeTree tree

lengthOfTree :: Tree Word8 -> Int64
lengthOfTree tree = BSL.length $ BB.toLazyByteString $ serializeTree tree

serializeTree :: Tree Word8 -> BB.Builder
serializeTree (Leaf value) = BB.word8 0x00 <> BB.word8 value
serializeTree (Node value left right) =
  let leftSerialized = serializeTree left
      rightSerialized = serializeTree right
      rightOffset = BB.word8 $ fromIntegral $ lengthOfTree left -- Points to next after calculating left
  in BB.word8 0x01 <> BB.word8 value <> rightOffset <> leftSerialized <> rightSerialized

printByteStringAsHex :: B.ByteString -> IO ()
printByteStringAsHex bs = putStrLn $ unwords $ chunksOf 2 $ show $ encode bs

printByteStringAsInt :: B.ByteString -> IO ()
printByteStringAsInt bs = putStrLn $ unwords $ map show $ B.unpack bs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

bsToIntList :: B.ByteString -> [Int]
bsToIntList bs = map ord (unpack bs)
