module Main where

import Tree (Tree(..))
import Serialization (serialize, printByteStringAsInt, printByteStringAsHex)
import Traversal (traverseRight, traverseLeft)
import View (Ptr(..), view, int64Value)


-- testing important 
import qualified Data.ByteString as B

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

  let offset= int64Value (B.take 8 (B.drop (2 + 24) serializedBST))
  print "This is offset"
  print offset

  print "This is index"
  print (B.index serializedBST 10)

  putStrLn "\n"
  print "Traversing right"
  let pointer = Ptr { buffer = serializedBST, position = 0 }
      rightMost = traverseRight pointer
  print rightMost

  putStrLn "\n"
  print "Traversing left"
  let leftMost = traverseLeft pointer
  print leftMost

-- module Main where


-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as BB
-- import qualified Data.ByteString.Lazy as BSL
-- import Data.ByteString.Base16 (encode)
-- import Data.Word ( Word8 )
-- import Data.Int (Int64)
-- import Data.ByteString.Char8 (unpack)
-- import Data.Char (ord)

-- data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show

-- chunksOf :: Int -> [a] -> [[a]]
-- chunksOf _ [] = []
-- chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- serialize:: Tree Word8 -> B.ByteString
-- serialize tree = B.toStrict $ BB.toLazyByteString $ serializeTree tree

-- lengthOfTree :: Tree Word8 -> Int64
-- lengthOfTree tree = BSL.length $ BB.toLazyByteString $ serializeTree tree

-- serializeTree :: Tree Word8 -> BB.Builder
-- serializeTree (Leaf value) = BB.word8 0x00 <> BB.word8 value
-- serializeTree (Node value left right) =
--   let leftSerialized = serializeTree left
--       rightSerialized = serializeTree right
--       rightOffset = BB.word8 $ fromIntegral $ lengthOfTree left -- Points to next after calculating left
--   in BB.word8 0x01 <> BB.word8 value <> rightOffset <> leftSerialized <> rightSerialized

-- printByteStringAsHex :: B.ByteString -> IO ()
-- printByteStringAsHex bs = putStrLn $ unwords $ chunksOf 2 $ show $ encode bs

-- printByteStringAsInt :: B.ByteString -> IO ()
-- printByteStringAsInt bs = putStrLn $ unwords $ map show $ B.unpack bs

-- traverseRight :: B.ByteString -> Int -> [Word8]
-- traverseRight bs offset
--   | offset >= B.length bs = []
--   | otherwise =
--     case B.index bs offset of
--       0x01 ->
--         let val = fromIntegral (B.index bs (offset + 1))
--             off = fromIntegral (B.index bs (offset + 2))
--         in val : traverseRight (B.drop (offset + 3) bs) off
--       0x00 -> [fromIntegral (B.index bs (offset + 1))]
--       _ -> error $ "Invalid byte in serialized tree: " ++ show (bsToIntList bs)

-- bsToIntList :: B.ByteString -> [Int]
-- bsToIntList bs = map ord (unpack bs)

-- main:: IO()
-- main = do
--     putStrLn "\n"
--     print "Welcome to testing of generalized trees"
--     let sampleBST = Node 20
--                (Node 5 (Leaf 2) (Leaf 10))
--                (Node 30 (Leaf 25) (Leaf 40))
--     print "This is a sample tree we will attempt to generalize"
--     print sampleBST

--     -- let sampleLength = BSL.length $ BB.toLazyByteString $ BB.word8 256
--     -- print sampleLength
--     putStrLn "\n"
--     print "Attempt 1: Serialize the tree"
--     let serializedBST = serialize sampleBST
--     printByteStringAsInt serializedBST
--     printByteStringAsHex serializedBST

--     putStrLn "\n"
--     print "Traversing right"
--     let rightMost = traverseRight serializedBST 0
--     print rightMost
