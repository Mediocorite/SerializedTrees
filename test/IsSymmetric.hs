module IsSymmetric where 

import Data.Word (Word8)
import Tree (Tree(..))
import View (Ptr(..), view, View(..))
import Deserializer (deserialize)

-- For Serialized Trees
isSymmetric :: Ptr (Tree Word8) -> Bool
isSymmetric ptr = symmetricHelper (view ptr) (view ptr)

symmetricHelper :: View Word8 -> View Word8 -> Bool
symmetricHelper (VLeaf v1) (VLeaf v2) = v1 == v2
symmetricHelper (VNode v1 left1 right1) (VNode v2 left2 right2) = 
    v1 == v2 && symmetricHelper (view left1) (view right2) && symmetricHelper (view right1) (view left2)
symmetricHelper _ _ = False

-- For Traditional Trees
traditionalIsSymmetric :: Eq a => Tree a -> Bool
traditionalIsSymmetric tree = traditionalSymmetricHelper tree tree

traditionalSymmetricHelper :: Eq a => Tree a -> Tree a -> Bool
traditionalSymmetricHelper (Leaf v1) (Leaf v2) = v1 == v2
traditionalSymmetricHelper (Node v1 l1 r1) (Node v2 l2 r2) = 
    v1 == v2 && traditionalSymmetricHelper l1 r2 && traditionalSymmetricHelper r1 l2
traditionalSymmetricHelper _ _ = False

-- Combining Deserializer with Traditional Algorithms
deserializeAndIsSymmetric:: Ptr (Tree Word8) -> Bool
deserializeAndIsSymmetric = traditionalIsSymmetric . deserialize