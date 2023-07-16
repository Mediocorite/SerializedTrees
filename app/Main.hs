module Main where

import Tree (Tree(..))
import Serialization (serialize, printByteStringAsInt, printByteStringAsHex)
import Traversal (traverseRight, traverseLeft)
import View (Ptr(..), view, int64Value)

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