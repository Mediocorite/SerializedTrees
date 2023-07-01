module Main where

-- Importing binary libraries to construct byte arrays

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Monoid

import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (putWord8)
import Data.Binary.Put (runPut, Put)
import Data.Word (Word8)


-- Traditional Tree function
data BST a = Leaf a | Node a (BST a) (BST a)


preorderToByteString :: Show a => BST a -> BS.ByteString
preorderToByteString = BSB.toByteString . mconcat . go
  where
    go (Leaf value) = [value]
    go (Node val left right) =
      BSB.intDec val : go left ++ go right


serializeBST :: BST Int -> [Word8]
serializeBST tree = BSL.unpack $ runPut (serializeTree tree)

-- There are two possible values given to serialize tree
serializeTree :: BST Int -> Put
-- Either we receive a leaf, in which case an imperative do block 
-- will simply place 0 first followed by value of leaf converted.
serializeTree (Leaf value) = do
  putWord8 0 -- Leaf indicator
  putWord8 (fromIntegral value)

-- The second option is more nuanced. We insert the 1 as the node 
-- indicator, then the offset value for the right most branch, encoding 
-- it in word8 as well, followed by the value of the node and finally 
-- a recursive call to handle left and right branches.
serializeTree (Node value left right) = do
  putWord8 1 -- Node indicator
  putWord8 (fromIntegral (offset right))
  putWord8 (fromIntegral value)
  serializeTree left
  serializeTree right

-- For the offset, as we are arranging the tree in pre order, we are 
-- using depth first search to return values. When we call offset on 
-- leaf, it will return 1. When we call a node, it will recursively 
-- calculate and accumulate the offset according to the depth.
offset :: BST a -> Int
offset (Leaf _) = 2
offset (Node _ left right) = 1 + offset left + offset right

traverseSkipLeft :: [Word8] -> [Int]
traverseSkipLeft [] = []
traverseSkipLeft (indicator : rest) =
  case indicator of
    0 -> []
    1 -> traverseSkipLeft (drop offset remainingBytes)
    where
      value = fromIntegral (head (drop 2 rest))
      offset = fromIntegral (head (drop 1 rest))
      remainingBytes = drop (offset + 2) rest


listToByteString :: [Int] -> BS.ByteString
listToByteString = BS.pack . map fromIntegral


main :: IO ()
main = do
  let tree = Node 20
               (Node 5 (Leaf 2) (Leaf 10))
               (Node 30 (Leaf 25) (Leaf 40))
  let serialized = serializeBST tree
  putStrLn "\n \n \n"
  putStrLn "This is the serialized form a sample bst"
  print serialized
  
  putStrLn "\n \n \n"
  let myList = [65, 66, 67] -- Example list of Int values
  let byteStringEx = listToByteString myList
  print byteStringEx -- Display the byteString
  -- putStrLn "\n \n \n"
  -- let preOrdered = Preorder tree
  -- putStrLn "This is preoreder form of program"
  -- print preOrdered

  putStrLn "\n \n \n"
  let values = traverseSkipLeft serialized
  putStrLn "Traversing the serialized BST while skipping left branches:"
  print values