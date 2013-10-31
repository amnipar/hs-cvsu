module CVSU.Segmentation
( divideUntilConsistent
, quadForestSegment
, quadForestSegmentByDeviation
, quadForestSegmentByOverlap
, quadForestSegmentByBoundaries
) where

import CVSU.Types
import CVSU.QuadTree
import CVSU.QuadForest

import Control.DeepSeq

-- | Goes through the listed trees, divides those that are inconsistent,
--   discards those that would become too small if divided
divideUntilConsistent :: (QuadTree -> Bool) -> QuadForest -> [QuadTree] -> IO [QuadTree]
divideUntilConsistent treeConsistent forest trees = divide ([], trees)
  where
    minSize = quadForestMinSize forest
    divide (rs,[]) = return rs
    divide (rs,ts) = do
      --print "divide"
      let
        (cs,is) = foldl separate ([],[]) ts
      ts' <- mapM (quadTreeDivide forest) is
      divide (rs ++ cs, concat ts')
    separate (cs,is) t
      | treeConsistent t = (t:cs,is)
      | quadTreeSize t >= 2 * minSize = (cs,t:is)
      | otherwise = (cs,is)

-- | Segments a forest by dividing trees until they are consistent by the
--   given measure, and by merging neighboring trees and segments that are
--   deemed equal in terms of the provided equivalence operation. Merging is
--   done by using disjoint sets implemented for image trees using Union-Find
--   algorithm. The result is a segmented forest where trees belonging to the
--   same segment have the same parent segment.
--   Note: the treeDivide and treeClassUnion operations cause side effects, so
--   this whole operation must be performed in full to leave the tree list in
--   consistent state, and in the current form, the algorithm is not safe to
--   parallelize (though in future it probably will be). Also the resulting
--   Haskell data structure is not persistent (may be later).
quadForestSegment :: (QuadTree -> Bool) -> (QuadTree -> QuadTree -> Bool)
    -> (ForestSegment -> ForestSegment -> Bool) -> QuadForest -> IO QuadForest
quadForestSegment treeConsistent treesEqual segmentsEqual forest = do
  -- first divide trees until they are consistent, and merge similar neighbors
  mergeTrees =<< mapM quadTreeSegmentInit =<<
      divideUntilConsistent treeConsistent forest (quadForestTrees forest)
  --print "merged trees"
  -- next, get the segments resulting from merging trees and merge similar neighbors
  ss <- quadForestGetSegments =<< quadForestRefreshSegments forest
  --print $ "got segments " ++ (show $ length ss)
  sr <- ss `deepseq` mergeSegments ss -- =<< quadForestGetSegments =<< quadForestRefreshSegments forest
  --print "merged segments"
  -- finally refresh the forest
  sr `deepseq` quadForestRefresh =<< quadForestRefreshSegments forest
  where
    minSize = quadForestMinSize forest
    -- go through all trees and find neighbors
    -- merge with neighboring trees that are equal
    mergeTrees [] = return ()
    mergeTrees (t:ts) = do
      --t' <- quadTreeSegmentInit t
      ns <- quadTreeNeighbors t
      --let ens = filter (treesEqual t) ns
      --print $ "neighbors " ++ (show $ length ns) ++ " equal " ++ (show $ length ens)
      mapM (quadTreeSegmentUnion t) $ filter (treesEqual t) ns
      mergeTrees ts
    -- go through all segments and find neighbors
    -- merge with neighboring segments that are equal
    mergeSegments [] = return ()
    mergeSegments (s:ss) = do
      ns <- forestSegmentNeighbors forest $! [s]
      --let ens = filter
      rs <- ns `deepseq` mapM (forestSegmentUnion s) $! filter (segmentsEqual s) ns
      ns `deepseq` rs `deepseq` mergeSegments ss

quadForestSegmentByDeviation :: Double -> Double -> QuadForest -> IO QuadForest
quadForestSegmentByDeviation threshold alpha f =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    r <- c'quad_forest_segment_with_deviation pforest
        (realToFrac threshold)
        (realToFrac alpha)
    if r /= c'SUCCESS
       then error $ "quadForestSegmentDeviation failed with " ++ (show r)
       else quadForestRefresh f

quadForestSegmentByOverlap :: Double -> Double -> Double -> QuadForest -> IO QuadForest
quadForestSegmentByOverlap alpha treeOverlap segmentOverlap forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_segment_with_overlap pforest
        (realToFrac alpha)
        (realToFrac treeOverlap)
        (realToFrac segmentOverlap)
    if r /= c'SUCCESS
      then error $ "quadForestSegmentEntropy failed with " ++ (show r)
      else quadForestRefresh forest

quadForestSegmentByBoundaries :: Int -> Double -> Double -> Double -> Double -> Bool -> Bool -> QuadForest -> IO QuadForest
quadForestSegmentByBoundaries rounds highBias lowFactor treeAlpha segmentAlpha useHysteresis usePruning forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_segment_with_boundaries pforest
        (fromIntegral rounds)
        (realToFrac highBias)
        (realToFrac lowFactor)
        (realToFrac treeAlpha)
        (realToFrac segmentAlpha)
        (cBool useHysteresis)
        (cBool usePruning)
    if r /= c'SUCCESS
      then error $ "Segment by boundaries failed with " ++ (show r)
      else quadForestRefresh forest
