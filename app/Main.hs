module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show




main:: IO()
main = do
    print "Welcome to testing of generalized trees"
    let sampleBST = Node 20
               (Node 5 (Leaf 2) (Leaf 10))
               (Node 30 (Leaf 25) (Leaf 40))
    print "This is a sample tree we will attempt to generalize"
    print sampleBST
