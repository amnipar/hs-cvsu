module Main where

import CVSU.Types
import CVSU.PixelImage hiding (writePixelImage)
import CVSU.QuadForest
import CVSU.Graph

import CV.Image
import CV.Thresholding
import CV.CVSU
import CV.CVSU.Drawing

import ReadArgs

valueAttribute :: IO (Attribute Int)
valueAttribute = attributeCreate 1 6

--componentLabel :: Attribute Set
--componentLabel = attributeCreate 2 $ Set 0

-- takes a label value and a node.
-- if node has same value as neighbor, makes a union with neighbor
-- using the setAttr attribute for storing the set membership.
-- if has different value, creates a new set and uses the setLabel.
-- returns the new setLabel and the new Node.
{-
unionWithSimilarNeighbor :: Eq a => Attribute a -> Attribute Set -> Int -> Node a-> (Int, Node (a,Set))
unionWithSimilarNeighbor valueAttr setAttr setLabel node
  | null similarNeighbors = (setLabel+1, attributeSetCreate setAttr setLabel node)
  | otherwise = (setLabel, attributeSetUnion setAttr node similarNeighbors)
  where
    similarNeighbors =
      filter (attributeCompare valueAttr (==) node) $ nodeNeighbors node

connectedComponentsGraph :: Image GrayScale D8 -> IO (Graph (Int, Set)
connectedComponentsGraph img = do
  pimg <- toPixelImage img
  graphFromImage pimg 1 Neighborhood4 binaryValue
  -}
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
{-
findConnectedComponents :: Eq a => Int -> Int -> Graph a -> Graph (a,Set)
findConnectedComponents valueAttr setAttr (Graph p nodes links) =
  Graph p (snd.mapAccumL (unionWithSimilarNeighbor valueAttr setAttr) 1 nodes) links
  -}
main = do
  (sourceFile, targetFile) <- readArgs
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ threshold MaxAndZero 127 $ unsafeImageTo8Bit img
  value <- valueAttribute
  g <- graphFromImage pimg 0 0 1 1 Neighborhood4 value
  saveImage targetFile img
  print "ok"
  --cg <- findConnectedComponents binaryValue componentLabel g
