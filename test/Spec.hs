-- Import the necessary modules and functions for testing
import Test.Hspec

-- Import the functions to be tested
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import Data.Binary.Put (runPut)
import Data.Binary.Get (runGet, getWord64be)

-- Functions from the library
import Tree (Tree(..))
import Serialization (serialize, serializeTree, printByteStringAsInt, printByteStringAsHex)
import Traversal (traverseRight, traverseLeft)
import View (Ptr(..), view, int64Value, View(..))
import Builder (runSerializer)

import Invert(invert, traditionalInvertTree)
import IsSymmetric(isSymmetric)
import LevelOrder(levelOrder)
import MaxDepth(maxDepth)
import PathSum(pathSum)
-- ...

main :: IO ()
main = hspec $ do
  describe "invert" $ do
    it "should invert a sample binary tree correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))
      let invertedTree = Node 1 (Node 5 (Leaf 7) (Leaf 6)) (Node 2 (Leaf 4) (Leaf 3))
      let samplePointer = Ptr (serialize sampleTree) 0
      let invertedPointer = Ptr (serialize invertedTree) 0
      runSerializer (invert samplePointer) `shouldBe` invertedPointer

    it "should invert a single-node binary tree correctly" $ do
      let sampleTree = Leaf 42
      let samplePointer = Ptr (serialize sampleTree) 0
      runSerializer (invert samplePointer) `shouldBe` samplePointer

  describe "isSymmetric" $ do
    it "should detect a symmetric binary tree correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 2 (Leaf 4) (Leaf 3))
      isSymmetric (Ptr (serialize sampleTree) 0) `shouldBe` True

    it "should detect a non-symmetric binary tree correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 2 (Leaf 5) (Leaf 3))
      isSymmetric (Ptr (serialize sampleTree) 0) `shouldBe` False

    it "should handle a single-node binary tree correctly" $ do
      let sampleTree = Leaf 42
      isSymmetric (Ptr (serialize sampleTree) 0) `shouldBe` True

  describe "levelOrder" $ do
    it "should perform level order traversal correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))
      let expectedTraversal :: [[Word8]]
          expectedTraversal = [[1], [2, 5], [3, 4, 6, 7]]
      levelOrder (Ptr (serialize sampleTree) 0) `shouldBe` expectedTraversal

    it "should handle a single-node binary tree correctly" $ do
      let sampleTree = Leaf 42
      let expectedTraversal ::[[Word8]] 
          expectedTraversal = [[42]]
      levelOrder (Ptr (serialize sampleTree) 0) `shouldBe` expectedTraversal

  describe "maxDepth" $ do
    it "should find the maximum depth of a sample binary tree correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))
      maxDepth (Ptr (serialize sampleTree) 0) `shouldBe` 3


  describe "pathSum" $ do
    it "should calculate the path sum of a sample binary tree correctly" $ do
      let sampleTree = Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))
      pathSum (Ptr (serialize sampleTree) 0) 6 `shouldBe` True
      pathSum (Ptr (serialize sampleTree) 0) 7 `shouldBe` True
      pathSum (Ptr (serialize sampleTree) 0) 12 `shouldBe` True

