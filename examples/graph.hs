{-#LANGUAGE TypeFamilies#-}
module Main where

import CVSU.Types
import CVSU.PixelImage hiding (writePixelImage)
import CVSU.QuadForest
import CVSU.Graph
import CVSU.Attribute
import CVSU.Set

import CV.Image
import CV.Filters
import CV.Thresholding
import CV.ColourUtils
import CV.CVSU
import CV.CVSU.Drawing

import Foreign.ForeignPtr
import ReadArgs

import Data.List
import Data.Ord

valueAttribute :: IO (Attribute Int)
valueAttribute = attributeCreate 1 0

componentAttribute :: IO (Attribute (Set ()))
componentAttribute = do
  s <- setCreate
  attributeCreate 2 s

valueGraph :: CGraph -> PixelImage -> Attribute Int -> IO (Graph Int)
valueGraph cg pimg value =
  --graphCreateFromImage pimg 5 5 8 8 Neighborhood4 value cg
  graphCreateFromImage pimg 0 0 1 1 Neighborhood4 value cg

-- takes a label value and a node.
-- if node has same value as neighbor, makes a union with neighbor
-- using the setAttr attribute for storing the set membership.
-- if has different value, creates a new set and uses the setLabel.
-- returns the new setLabel and the new Node.
unionWithSimilarNeighbors :: (Eq a, AttribValue a, AttribValue b) =>
    Attribute (a,Set()) -> Node b -> IO ()
unionWithSimilarNeighbors pairAttr node = do
  (val,set) <- getAttribute pairAttr node
  neighbors <- nodeNeighbors node
  mapM_ (unionWithSimilar pairAttr val set) neighbors
  where
    unionWithSimilar pair val set node = do
      (nval,nset) <- getAttribute pair node
      if (val == nval)
         then setUnion set nset
         else setNull

findConnectedComponents :: (Eq a, AttribValue a, AttribValue b) =>
  Attribute a -> Attribute (Set ()) -> [Node b] -> IO [Node (a,Set())]
findConnectedComponents valueAttr setAttr nodes = do
  pairAttr <- attributePair valueAttr setAttr
  mapM_ (unionWithSimilarNeighbors pairAttr) nodes
  mapM (createAttributed pairAttr) $ map nodePtr nodes

removeLinkSmaller :: Attribute (Set ()) -> Double -> Link -> IO ()
removeLinkSmaller setAttr t link =
  if linkWeight link < t
    then do
      set1 <- getAttribute setAttr $ linkFrom link
      set2 <- getAttribute setAttr $ linkTo link
      if setId set1 /= setId set2
        then do
          setUnion set1 set2
          return ()
        else return ()
    else return ()

removeLink :: Attribute (Set ()) -> Link -> IO ()
removeLink setAttr link = do
  set1 <- getAttribute setAttr $ linkFrom link
  set2 <- getAttribute setAttr $ linkTo link
  if setId set1 /= setId set2
    then do
      setUnion set1 set2
      return ()
    else return ()

minimumSpanningForest :: (Num a, AttribValue a, AttribValue b) =>
  Attribute a -> Attribute (Set ()) -> Double -> Graph b -> IO (Graph (a,Set()))
minimumSpanningForest valueAttr setAttr t graph = do
  pairAttr <- attributePair valueAttr setAttr
  mapM_ (removeLinkSmaller setAttr t) $
      sortBy (comparing linkWeight) $ links graph
  graphGetAttribute pairAttr graph

minimumSpanningTrees :: (Num a, AttribValue a, AttribValue b) =>
  Attribute a -> Attribute (Set ()) -> Int -> Graph b -> IO (Graph (a,Set()))
minimumSpanningTrees valueAttr setAttr n graph = do
  pairAttr <- attributePair valueAttr setAttr
  mapM_ (removeLink setAttr) $ take ((length $ links graph) - (n-1)) $
      sortBy (comparing linkWeight) $ links graph
  graphGetAttribute pairAttr graph

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

main = do
  (sourceFile, n, targetFile) <- readArgs
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ unsafeImageTo8Bit $ stretchHistogram $ gaussian (5,5) img
  -- threshold MaxAndZero 127 $
  value <- valueAttribute
  comp <- componentAttribute
  cg <- newCGraph
  vgraph <- valueGraph cg pimg value
  sgraph <- graphAddAttribute comp vgraph
  fgraph <- minimumSpanningForest value comp n sgraph
  --fgraph <- minimumSpanningTrees value comp n sgraph
  vals <- mapM (getAttribute value) (nodes fgraph)
  --cnodes <- findConnectedComponents value comp $ nodes sgraph
  sets <- mapM (getAttribute comp) (nodes fgraph)
  let
    vpicker = createColorPicker (False,(0,0,0),(1,0,0)) vals
    spicker = createColorPicker () sets
  --saveImage targetFile $ drawGraphColor vpicker value fgraph $ grayToRGB img
  --saveImage targetFile $ drawGraphColor spicker comp fgraph $ grayToRGB img
  saveImage targetFile $ drawGraphImageColor spicker 1 comp fgraph
