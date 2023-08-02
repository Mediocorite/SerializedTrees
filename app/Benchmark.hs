{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.DeepSeq (  NFData(..) )
import Criterion.Main ( defaultMain, bench, bgroup, nf )
import Serialization (serialize)
import Invert (invert, traditionalInvertTree)
import Tree (generateTrees, Tree(..), calculateDepth)
import qualified Data.ByteString as B
import View (Ptr(..))
import Data.Word (Word8)

-- Add the NFData instance for Tree
instance NFData a => NFData (Tree a) where
  rnf :: Tree a -> ()
  rnf (Leaf a) = rnf a
  rnf (Node a l r) = rnf a `seq` rnf l `seq` rnf r

-- Add the NFData instance for Ptr
instance NFData (Ptr (Tree a)) where
  rnf :: Ptr (Tree a) -> ()
  rnf (Ptr bs p) = rnf bs `seq` rnf p

writeTrees :: [(Tree Int, Int)] -> IO ()
writeTrees trees = writeFile "trees.bin" (show trees)

readTrees :: IO [(Tree Int, Int)]
readTrees = read <$> readFile "trees.bin"

writeSerializedTrees :: [B.ByteString] -> IO ()
writeSerializedTrees trees = B.writeFile "serializedTrees.bin" (B.concat trees)

readSerializedTrees :: IO [[B.ByteString]]
readSerializedTrees = return . B.split 0 <$> B.readFile "serializedTrees.bin"

prepareTrees :: IO [(Tree Int, Ptr (Tree Word8), Int)]
prepareTrees = do
  trees <- generateTrees 255
  writeTrees trees
  let serializedTrees = map (serialize . fmap fromIntegral . fst) trees
  writeSerializedTrees serializedTrees
  let ptrTrees = map (\bs -> Ptr {buffer = bs, position = 0}) serializedTrees
  return $ zip3 (map fst trees) ptrTrees (map (calculateDepth . fst) trees)

main :: IO ()
main = do
  trees <- prepareTrees
  defaultMain [
      bgroup "invert" [ bench (show depth) $ nf invert ptr | (tree, ptr, depth) <- trees ],
      bgroup "traditionalInvertTree" [ bench (show depth) $ nf traditionalInvertTree tree | (tree, ptr, depth) <- trees ]
    ]
