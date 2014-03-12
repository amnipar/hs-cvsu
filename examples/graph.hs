module Main where

import CVSU.Types
import CVSU.PixelImage hiding (writePixelImage)
import CVSU.QuadForest
import CVSU.Graph

import CV.Image
import CV.CVSU
import CV.CVSU.Drawing

import ReadArgs

binaryValue :: Attribute Int
binaryValue = attributeLabel 1 0

componentLabel :: Attribute Set
componentLabel = attributeLabel 2 $ Set 0

-- takes a label value and a node.
-- if node has same value as neighbor, makes a union with neighbor
-- using the setAttr attribute for storing the set membership.
-- if has different value, creates a new set and uses the setLabel.
-- returns the new setLabel and the new Node.
unionWithSimilarNeighbor :: Int -> Int -> Int -> Node -> (Int, Node)
unionWithSimilarNeighbor valueAttr setAttr setLabel node
  | null similarNeighbors = (setLabel+1, attributeSetCreate setAttr setLabel node)
  | otherwise = (setLabel, attributeSetUnion setAttr node similarNeighbors)
  where
    similarNeighbors =
      filter (attributeCompare valueAttr (==) node) $ nodeNeighbors node

-- takes a graph and labels neighboring nodes that have the same value for a
-- the attribute valueAttr with the same label value in attribute labelAttr
--
-- goes through all nodes.
-- checks the neighbors above and to the left.
-- if neighbor has the same value for valueAttr, set node's labelAttr to same
-- value as the neighbor.
-- this should work so, that if the neighbor doesn't yet have labelAttr, it is
-- added also to the neighbor using the next label value.
-- need a recursion that stores the current label value.
-- need a label-setting function that returns the current/next? label value.
-- actually shouldn't label nodes but make a union with similar neighbors.

findConnectedComponents :: Int -> Int -> Graph -> Graph
findConnectedComponents valueAttr setAttr (Graph nodes e i) =
  Graph (snd.mapAccumL (unionWithSimilarNeighbor valueAttr setAttr) 0 nodes) e i

main = do
  (maxSize, minSize, sourceFile, targetFile) <- readArgs
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ threshold 128 $ unsafeImageTo8Bit $ img
  g <- graphFromImage pimg 1 Neighborhood4 binaryValue
  cg <- findConnectedComponents binaryValue componentLabel g

