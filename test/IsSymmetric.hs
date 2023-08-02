module IsSymmetric where 

import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as BB
-- import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)

import Tree (Tree(..))
import View (Ptr(..), view, int64Value, View(..))
isSymmetricNodes :: B.ByteString -> B.ByteString -> Bool
isSymmetricNodes node1 node2 = node1 == B.reverse node2

isSymmetric :: Ptr (Tree Word8) -> Bool
isSymmetric ptr = case view ptr of
    VLeaf _ -> True  -- A leaf node is always symmetric
    VNode _ leftPtr rightPtr ->
        let leftSubtree = B.take (10 + int64Value (B.take 8 (B.drop 2 (buffer rightPtr)))) (buffer leftPtr)
            rightSubtree = B.take (10 + int64Value (B.take 8 (B.drop 2 (buffer leftPtr)))) (buffer rightPtr)
        in isSymmetricNodes leftSubtree rightSubtree && isSymmetric (Ptr (B.drop 10 (buffer leftPtr)) 0) && isSymmetric (Ptr (B.drop 10 (buffer rightPtr)) 0)