module Main where

import Tree (Tree(..))
import Serialization (serialize, serializeTree, printByteStringAsInt, printByteStringAsHex)
import Traversal (traverseRight, traverseLeft)
import View (Ptr(..), view, int64Value, View(..))

-- Testing
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL

import Data.Word (Word8)
import Data.ByteString.Base16 (encode)


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