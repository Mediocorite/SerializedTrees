module Main where

import Tree (Tree(..))
import Serialization (serialize, printByteStringAsInt, printByteStringAsHex, serializeTree)
import Traversal (traverseRight, traverseLeft)
import View (Ptr(..), view, int64Value, View(..))

-- Testing
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL

import Data.ByteString.Base16 (encode)
invert :: B.ByteString -> B.ByteString
invert bs = case view (Ptr bs 0) of
    VLeaf v -> B.pack [0x00, v]  -- Serialize a leaf node directly
    VNode v leftPtr rightPtr ->
        let invertedLeft = invert (B.drop 10 bs)  -- Drop the first 10 bytes to skip the node header
            invertedRight = invert (B.drop (10 + int64Value (B.take 8 (B.drop 2 bs))) bs) -- Drop the first 10 + offset bytes
            invertedOffset = BB.toLazyByteString (BB.word64BE (fromIntegral (B.length invertedRight)))
        in B.concat [B.pack [0x01, v], BSL.toStrict invertedOffset, invertedRight, invertedLeft]

main :: IO ()
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
  let pointer = Ptr { buffer = serializedBST, position = 0 }
      rightMost = traverseRight pointer
  print rightMost

  putStrLn "\n"
  print "Traversing left"
  let leftMost = traverseLeft pointer
  print leftMost

  putStrLn "\n"
  print "Inverting tree"
  let inverted = invert serializedBST
  printByteStringAsInt inverted