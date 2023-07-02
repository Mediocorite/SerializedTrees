module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Base16 (encode)
import Data.Word ( Word8 )
import Data.Int (Int64)
import GHC.Event.Windows.FFI (pokeOffsetOverlapped)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

serialize:: Tree Word8 -> B.ByteString
serialize tree = B.toStrict $ BB.toLazyByteString $ serializeTree tree

serializedLength :: Tree Word8 -> Int64
serializedLength tree = BSL.length $ BB.toLazyByteString $ serializeTree tree

serializeTree :: Tree Word8 -> BB.Builder
serializeTree (Leaf value) = BB.word8 0x00 <> BB.word8 value
serializeTree (Node value left right) =
  let leftSerialized = serializeTree left
      rightSerialized = serializeTree right
      rightOffset = BB.word8 $ fromIntegral $ serializedLength right -- +5 to account for tag and offset fields
  in BB.word8 0x01 <> rightOffset <> BB.word8 value <> leftSerialized <> rightSerialized

printByteStringAsHex :: B.ByteString -> IO ()
printByteStringAsHex bs = putStrLn $ unwords $ chunksOf 2 $ show $ encode bs

printByteStringAsInt :: B.ByteString -> IO ()
printByteStringAsInt bs = putStrLn $ unwords $ map show $ B.unpack bs

traverseRight :: B.ByteString -> Int -> [Int]
traverseRight bs offset = 
  if offset >= B.length bs then
    []
  else 
    case B.index bs offset of 
      0x01 -> 
        let off = fromIntegral (B.index bs offset + 1)
            val = fromIntegral (B.index bs offset + 2)
        in ([val] <> traverseRight bs off)
      0x00 -> [fromIntegral (B.index bs offset + 1)]

main:: IO()
main = do
    putStrLn "\n"
    print "Welcome to testing of generalized trees"
    let sampleBST = Node 20
               (Node 5 (Leaf 2) (Leaf 10))
               (Node 30 (Leaf 25) (Leaf 40))
    print "This is a sample tree we will attempt to generalize"
    print sampleBST

    putStrLn "\n"
    print "Attempt 1: Serialize the tree"
    let serializedBST = serialize sampleBST
    printByteStringAsInt serializedBST
    printByteStringAsHex serializedBST

    putStrLn "\n"
    print "Traversing right"
    let rightMost = traverseRight serializedBST 0
    print rightMost
