{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import qualified Data.ByteString as B
import Control.DeepSeq (  NFData(..) )
import Criterion.Main ( defaultMain, bench, bgroup, nf )
import Serialization (serialize)
import Tree (generateTrees, Tree(..))
import View (Ptr(..))
import Data.Word (Word8)

-- Benchmarking the following algorithms; Comment unneeded
-- import Invert (invert, traditionalInvertTree,deserializeAndInvert)
import IsSymmetric (isSymmetric, traditionalIsSymmetric, deserializeAndIsSymmetric)
-- import LevelOrder (levelOrder, traditionalLevelOrder, deserializeTraditionalLevelOrder)
-- import MaxDepth (maxDepth, traditionalMaxDepth, deserializeMaxDepth)
-- import PathSum (pathSum, traditionalPathSumHelper)


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
  -- Doing a simple test benchmark
  trees <- generateTrees 255 100
  writeTrees trees
  let serializedTrees = map (serialize . fmap fromIntegral . fst) trees
  writeSerializedTrees serializedTrees
  let ptrTrees = map (\bs -> Ptr {buffer = bs, position = 0}) serializedTrees
  return $ zip3 (map fst trees) ptrTrees (map snd trees) 



main :: IO ()
main = do
  trees <- prepareTrees
  defaultMain [
      -- Uncomment the following three for Invert Benchmark
      -- bgroup "traditionalInvertTree" [ bench (show depth) $ nf traditionalInvertTree tree | (tree, _, depth) <- trees ],
      -- bgroup "deserializeAndInvert" [ bench (show depth) $ nf deserializeAndInvert ptr | (_, ptr, depth) <- trees ],
      -- bgroup "invert" [ bench (show depth) $ nf invert ptr | (_, ptr, depth) <- trees ]
      -- Uncomment the following three for IsSymmetric Benchmark
      bgroup "traditionalIsSymmetric" [ bench (show depth) $ nf traditionalIsSymmetric tree | (tree, _, depth) <- trees ],
      bgroup "deserializeAndIsSymmetric" [ bench (show depth) $ nf deserializeAndIsSymmetric ptr | (_, ptr, depth) <- trees ],
      bgroup "IsSymmetric" [ bench (show depth) $ nf isSymmetric ptr | (_, ptr, depth) <- trees ]
      -- Uncomment the following three for Level Order Benchmark
      -- bgroup "traditionalLevelOrder" [ bench (show depth) $ nf traditionalLevelOrder tree | (tree,_, depth) <- trees ],
      -- bgroup "deserializeTraditionalLevelOrder" [ bench (show depth) $ nf deserializeTraditionalLevelOrder ptr | (_, ptr, depth) <- trees ],
      -- bgroup "levelOrder" [ bench (show depth) $ nf levelOrder ptr | (_, ptr, depth) <- trees ]
      -- Uncomment the following three for Max Depth Benchmark
      -- bgroup "traditionalMaxDepth" [ bench (show depth) $ nf traditionalMaxDepth tree | (tree, _, depth) <- trees ],
      -- bgroup "deserializeTraditionalLevelOrder" [ bench (show depth) $ nf deserializeMaxDepth ptr | (_, ptr, depth) <- trees ],
      -- bgroup "maxDepth" [ bench (show depth) $ nf maxDepth ptr | (_, ptr, depth) <- trees ]
      -- Uncomment the following three for Invert Benchmark
      -- bgroup "pathSum" [ bench (show depth) $ nf pathSum ptr | (tree, ptr, depth) <- trees ],
      -- bgroup "traditionalPathSumHelper" [ bench (show depth) $ nf traditionalPathSumHelper tree | (tree, ptr, depth) <- trees ]
    ]
