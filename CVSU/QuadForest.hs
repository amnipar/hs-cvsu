{-# LANGUAGE OverlappingInstances, TupleSections, ScopedTypeVariables #-}

module CVSU.QuadForest
( QuadTree(..)
, QuadForest(..)
, ForestSegment(..)
, forestSegmentUnion
, forestSegmentNeighbors
, ForestEdge(..)
, segmentLeft
, segmentTop
, segmentRight
, segmentBottom
, quadTreeHasEdge
, quadTreeHasVEdge
, quadTreeHasHEdge
, quadTreeChildStat
, quadTreeNeighborhoodStat
, quadTreeDivide
, quadTreeNeighbors
, quadTreeEdgeResponse
, quadTreeChildEdgeResponse
, quadTreeSegmentInit
, quadTreeSegmentUnion
, quadTreeSegmentFind
, withQuadForest
, quadForestCreate
, quadForestUpdate
, quadForestRefresh
, quadForestRefreshSegments
, quadForestGetTopLevelTrees
, quadForestGetTree
, divideUntilConsistent
, quadForestSegment
, quadForestSegmentByDeviation
, quadForestSegmentByOverlap
, quadForestSegmentByBoundaries
, quadForestFindEdges
, quadForestFindBoundaries
, quadForestParse
, quadForestSegmentEdges
, quadForestGetSegments
, quadForestGetBoundaries
, quadForestGetSegmentTrees
, quadForestGetSegmentMask
, quadForestGetSegmentBoundary
, quadForestHighlightSegments
, quadForestDrawImage
, quadForestDrawTrees
, mapDeep
, createEdgeChain
, getPathSniffers
, getForestLinks
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.List
import CVSU.Bindings.QuadForest
import CVSU.Types
import CVSU.PixelImage
import CVSU.List

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Array
import Foreign.Concurrent
import System.IO.Unsafe
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Exception hiding (block)
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Float
import Debug.Trace

data ForestSegment =
  ForestSegment
  { segmentPtr :: Ptr C'quad_forest_segment
  , segmentX :: Int
  , segmentY :: Int
  , segmentW :: Int
  , segmentH :: Int
  , segmentStat :: Statistics
  , segmentDevMean :: Double
  , segmentDevDev :: Double
  , segmentHasBoundary :: Bool
  , segmentColor :: (Float,Float,Float)
  } deriving Eq

segmentLeft = segmentX
segmentTop = segmentY
segmentRight s = segmentX s + segmentW s
segmentBottom s = segmentY s + segmentH s

hForestSegment (C'quad_forest_segment p _ x1 y1 x2 y2 stat m d b c1 c2 c3) =
  ForestSegment
    p
    (fromIntegral x1)
    (fromIntegral y1)
    (fromIntegral $ x2-x1)
    (fromIntegral $ y2-y1)
    (hStatistics stat)
    (realToFrac m)
    (realToFrac d)
    (hBool b)
    (toColor c1 c2 c3)
  where
    makeC = (/255).realToFrac
    toColor c1 c2 c3 = (makeC c1, makeC c2, makeC c3)

data ForestEdge =
  ForestEdge
  { edgePtr :: Ptr C'quad_forest_edge
  , edgeDX :: Double
  , edgeDY :: Double
  , edgeMag :: Double
  , edgeAng :: Double
  , edgeMean :: Double
  , edgeDev :: Double
  , edgeFound :: Bool
  , edgeDir :: Direction
  } deriving Eq

hForestEdge (C'quad_forest_edge _ p _ _ _ _ _ _ dx dy mag ang mean dev has_edge _ dir) =
  ForestEdge
    p
    (realToFrac dx)
    (realToFrac dy)
    (realToFrac mag)
    (realToFrac ang)
    (realToFrac mean)
    (realToFrac dev)
    (hBool has_edge)
    (hDirection dir)

forestSegmentFromPtr :: Ptr C'quad_forest_segment -> IO ForestSegment
forestSegmentFromPtr psegment
  | psegment == nullPtr = error "null pointer in forestSegmentFromPtr"
  | otherwise = do
    segment <- peek psegment
    return $ hForestSegment segment

forestSegmentFromListItem :: Ptr C'list_item -> IO ForestSegment
forestSegmentFromListItem pitem = do
  item <- peek pitem
  psegment <- peek $ castPtr $ c'list_item'data item
  forestSegmentFromPtr $ castPtr psegment

data QuadTree = EmptyQuadTree |
  QuadTree
  { quadTreePtr :: Ptr C'quad_tree
  , quadTreeX :: Int
  , quadTreeY :: Int
  , quadTreeSize :: Int
  , quadTreeLevel :: Int
  , quadTreeStat :: Statistics
  , quadTreeEdge :: ForestEdge
  , quadTreeSegment :: ForestSegment
  , quadTreeChildNW :: QuadTree
  , quadTreeChildNE :: QuadTree
  , quadTreeChildSW :: QuadTree
  , quadTreeChildSE :: QuadTree
  } deriving Eq

quadTreeHasEdge  = edgeFound . quadTreeEdge
quadTreeHasVEdge t =
  (quadTreeHasEdge t) && ((==DirV) $ edgeDir $ quadTreeEdge t)
quadTreeHasHEdge t =
  (quadTreeHasEdge t) && ((==DirH) $ edgeDir $ quadTreeEdge t)

quadTreeFromPtr :: Ptr C'quad_tree -> IO QuadTree
quadTreeFromPtr ptree
  | ptree == nullPtr = return EmptyQuadTree
  | otherwise        = do
    (C'quad_tree x y s l stat seg edge _ _ _ nw ne sw se _ _ _ _ _ _ _ _ _ _) <- peek ptree
    tnw <- quadTreeFromPtr nw
    tne <- quadTreeFromPtr ne
    tsw <- quadTreeFromPtr sw
    tse <- quadTreeFromPtr se
    return $ QuadTree ptree
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral s)
      (fromIntegral l)
      (hStatistics stat)
      (hForestEdge edge)
      (hForestSegment seg)
      tnw tne tsw tse

-- | Creates an image tree from a list item by casting and converting
quadTreeFromListItem :: Ptr C'list_item -> IO QuadTree
quadTreeFromListItem pitem = do
  item <- peek pitem
  ptree <- peek $ castPtr $ c'list_item'data item
  quadTreeFromPtr $ castPtr ptree

-- | Divides the tree in four equal parts in quad-tree fashion. Causes side
--   effects as the original tree will have four child trees after this
--   operation.
quadTreeDivide :: QuadForest -> QuadTree -> IO [QuadTree]
quadTreeDivide forest tree =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_tree_divide pforest (quadTreePtr tree)
    if r /= c'SUCCESS
      then error $ "Divide tree failed with " ++ (show r)
      else do
        ctree <- peek (quadTreePtr tree)
        liftM (filter (/=EmptyQuadTree)) $ mapM quadTreeFromPtr
          [ c'quad_tree'nw ctree
          , c'quad_tree'ne ctree
          , c'quad_tree'sw ctree
          , c'quad_tree'se ctree ]

quadTreeChildStat :: QuadForest -> QuadTree -> IO [Statistics]
quadTreeChildStat f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      allocChildArray :: IO (Ptr C'quad_tree)
      allocChildArray = mallocArray 4
    pchildren <- allocChildArray
    r <- c'quad_tree_get_child_statistics pforest (quadTreePtr t) pchildren
    if r /= c'SUCCESS
      then error $ "Get child statistics failed with " ++ (show r)
      else do
        children <- (peekArray 4 pchildren)
        return $ map (hStatistics.c'quad_tree'stat) children

quadTreeNeighborhoodStat :: QuadForest -> Double -> QuadTree -> IO Statistics
quadTreeNeighborhoodStat f m t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      stat = C'statistics 0 0 0 0 0 0
    with stat $ \pstat -> do
      r <- c'quad_tree_get_neighborhood_statistics pforest (quadTreePtr t) pstat (realToFrac m)
      if r /= c'SUCCESS
         then error $ "Get neighborhood statistics failed with " ++ (show r)
         else do
           nstat <- peek pstat
           return $ hStatistics nstat

-- | Finds the current neighbors of the given tree.
quadTreeNeighbors :: QuadTree -> IO [QuadTree]
quadTreeNeighbors tree = do
  flist <- allocList
  withForeignPtr flist $ \plist -> do
    r <- c'quad_tree_get_neighbors plist (quadTreePtr tree)
    if r /= c'SUCCESS
      then error $ "Getting tree neighbors failed with " ++ (show r)
      else createList plist quadTreeFromListItem

quadTreeEdgeResponse :: QuadForest -> QuadTree -> IO (Double,Double)
quadTreeEdgeResponse f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
        dx :: CDouble
        dx = 0
        dy :: CDouble
        dy = 0
    with dx $ \pdx ->
      with dy $ \pdy -> do
        r <- c'quad_tree_get_edge_response pforest (quadTreePtr t) pdx pdy
        if r /= c'SUCCESS
          then error $ "Get tree edge response failed with " ++ (show r)
          else do
            rdx <- peek pdx
            rdy <- peek pdy
            return (realToFrac rdx, realToFrac rdy)
            --return $ sqrt $ (realToFrac rdx)**2 + (realToFrac rdy)**2

quadTreeChildEdgeResponse :: QuadForest -> QuadTree -> IO [(Double,Double)]
quadTreeChildEdgeResponse f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      allocDArray :: IO (Ptr CDouble)
      allocDArray = mallocArray 4
    pdx <- allocDArray
    pdy <- allocDArray
    r <- c'quad_tree_get_child_edge_response pforest (quadTreePtr t) pdx pdy
    if r /= c'SUCCESS
      then error $ "Get child edge response failed with " ++ (show r)
      else do
        xs <- (peekArray 4 pdx)
        ys <- (peekArray 4 pdy)
        return $ zip (map realToFrac xs) (map realToFrac ys)

-- | Initializes a disjunctive set for the tree by setting the parent to
--   self and rank to zero.
quadTreeSegmentInit :: QuadTree -> IO QuadTree
quadTreeSegmentInit t = do
  c'quad_tree_segment_create (quadTreePtr t)
  quadTreeFromPtr $ quadTreePtr t

-- | Union part of the Union-Find disjunctive set algorithm. Creates a union
--   of two sets by assigning the parent of the higher ranking tree as the
--   parent of the union. As the result, the two trees in question will have
--   the same classId. Loses persistency, as the classId is changed in the
--   underlying c structure, so previous versions of the trees that still
--   point to the same c structure will have different classId.
quadTreeSegmentUnion :: QuadTree -> QuadTree -> IO ()
quadTreeSegmentUnion t1 t2 =
  c'quad_tree_segment_union (quadTreePtr t1) (quadTreePtr t2)

-- | Creates a union of two segments that are larger than trees.
forestSegmentUnion :: ForestSegment -> ForestSegment -> IO ()
forestSegmentUnion s1 s2 =
  c'quad_forest_segment_union (segmentPtr s1) (segmentPtr s2)

-- | Find part of the Union-Find disjunctive set algorithm. Finds the pointer
--   of the parent tree, caches it in the c structure, and creates an Integer
--   from the pointer. May cause side effects as the parent pointer is
--   cached to shorten the search paths for better efficiency.
quadTreeSegmentFind :: QuadTree -> IO (Integer)
quadTreeSegmentFind t = do
  tid <- c'quad_tree_segment_get (quadTreePtr t)
  return $ fromIntegral tid

data QuadForest =
  QuadForest
  { quadForestPtr :: !(ForeignPtr C'quad_forest)
  , quadForestImage :: PixelImage
  , quadForestRows :: Int
  , quadForestCols :: Int
  , quadForestSegments :: Int
  , quadForestMaxSize :: Int
  , quadForestMinSize :: Int
  , quadForestDX :: Int
  , quadForestDY :: Int
  , quadForestTrees :: ![QuadTree]
  }

-- allocate image_tree_forest structure using c function and foreign pointers
-- image_tree_forest_alloc is used for allocating the image struct
-- image_tree_forest_free is specified as finalizer
quadForestAlloc :: IO (ForeignPtr C'quad_forest)
quadForestAlloc = do
  ptr <- c'quad_forest_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'quad_forest_free ptr)
    else error "Memory allocation failed in quadForestAlloc"

quadForestInit :: Int -> Int -> PixelImage -> IO QuadForest
quadForestInit maxSize minSize image = do
  fforest <- quadForestAlloc
  withForeignPtr fforest $ \pforest ->
    withForeignPtr (imagePtr image) $ \pimage -> do
      r1 <- c'quad_forest_create pforest pimage (fromIntegral maxSize) (fromIntegral minSize)
      if r1 /= c'SUCCESS
        then error $ "Quad forest create failed with " ++ (show r1)
        else do
          C'quad_forest{
            c'quad_forest'rows = r,
            c'quad_forest'cols = c,
            c'quad_forest'segments = s,
            c'quad_forest'tree_max_size = smax,
            c'quad_forest'tree_min_size = smin,
            c'quad_forest'dx = dx,
            c'quad_forest'dy = dy
          } <- peek pforest
          return $ QuadForest fforest image
            (fromIntegral r)
            (fromIntegral c)
            (fromIntegral s)
            (fromIntegral smax)
            (fromIntegral smin)
            (fromIntegral dx)
            (fromIntegral dy)
            []

-- | Creates the forest structure, given an image and number of rows and
--   columns in the root tree array.
quadForestCreate :: Int -> Int -> PixelImage -> IO QuadForest
quadForestCreate maxSize minSize image =
  quadForestUpdate =<< quadForestInit maxSize minSize image

-- | Updates the underlying integral images of the forest, invalidating the
--   tree contents; thus it requires a refresh. Can be used for processing
--   video, if the video frame buffer content is copied directly to the
--   memory block containing the image data used for creating the forest.
--   Also used by the create function.
quadForestUpdate :: QuadForest -> IO QuadForest
quadForestUpdate f = do
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    result <- c'quad_forest_update pforest
    if result /= c'SUCCESS
      then error $ "Forest update failed with " ++ (show result)
      else quadForestRefresh f

-- | Refreshes the forest structure by reading it in full from the underlying
--   c structure. Required after operations causing side effects to ensure
--   consistency of the whole data structure.
quadForestRefresh :: QuadForest -> IO QuadForest
quadForestRefresh forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    C'quad_forest{
      c'quad_forest'rows = r,
      c'quad_forest'cols = c,
      c'quad_forest'segments = s,
      c'quad_forest'tree_max_size = smax,
      c'quad_forest'tree_min_size = smin,
      c'quad_forest'dx = dx,
      c'quad_forest'dy = dy,
      c'quad_forest'roots = proots
    } <- peek pforest
    rs <- peekArray (fromIntegral $ r*c) proots
    ts <- mapM quadTreeFromPtr rs
    return $ QuadForest
      (quadForestPtr forest)
      (quadForestImage forest)
      (fromIntegral r)
      (fromIntegral c)
      (fromIntegral s)
      (fromIntegral smax)
      (fromIntegral smin)
      (fromIntegral dx)
      (fromIntegral dy)
      ts

quadForestRefreshSegments :: QuadForest -> IO QuadForest
quadForestRefreshSegments forest = do
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_refresh_segments pforest
    if r /= c'SUCCESS
      then error $ "Refresh segments failed with " ++ (show r)
      else quadForestRefresh forest

withQuadForest :: QuadForest -> (QuadForest -> IO a) -> IO a
withQuadForest f op =
  withForeignPtr (quadForestPtr f) $ \_ -> do
    r <- op f
    touchForeignPtr (quadForestPtr f)
    return $! r

quadForestGetTopLevelTrees :: QuadForest -> [QuadTree]
quadForestGetTopLevelTrees forest = concatMap getTrees $ quadForestTrees forest
  where
    getTrees EmptyQuadTree = []
    getTrees t
      | null cs = [t]
      | otherwise = cs
      where
        cs = concatMap getTrees $
          [ quadTreeChildNW t
          , quadTreeChildNE t
          , quadTreeChildSW t
          , quadTreeChildSE t ]

-- | Extract the given root tree from the forest
quadForestGetTree :: QuadForest -> (Int,Int) -> IO QuadTree
quadForestGetTree f (x,y) =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    forest <- peek pforest
    root <- peek $ advancePtr (c'quad_forest'roots forest) (y * (quadForestCols f) + x)
    quadTreeFromPtr root

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

quadForestFindEdges :: Int -> Double -> Direction -> QuadForest -> IO QuadForest
quadForestFindEdges rounds bias dir forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_find_edges pforest
        (fromIntegral rounds)
        (realToFrac bias)
        (cDirection dir)
    if r /= c'SUCCESS
      then error $ "quadForestFindEdges failed with " ++ (show r)
      else quadForestRefresh forest

quadForestFindBoundaries :: Int -> Double -> Int -> QuadForest -> IO QuadForest
quadForestFindBoundaries rounds bias minLength forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_find_boundaries pforest
        (fromIntegral rounds)
        (realToFrac bias)
        (fromIntegral minLength)
    if r /= c'SUCCESS
      then error $ "quadForestFindBoundaries failed with " ++ (show r)
      else quadForestRefresh forest

quadForestParse :: Int -> Double -> Int -> QuadForest -> IO QuadForest
quadForestParse rounds bias minLength forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_parse pforest
        (fromIntegral rounds)
        (realToFrac bias)
        (fromIntegral minLength)
    if r /= c'SUCCESS
      then error $ "quadForestParse failed with " ++ (show r)
      else quadForestRefresh forest

quadForestSegmentEdges :: Int -> Double -> Direction -> Int -> Double
      -> Direction -> Direction -> QuadForest -> IO QuadForest
quadForestSegmentEdges detectRounds detectBias detectDir propRounds
      propThreshold propDir mergeDir forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_segment_edges pforest
        (fromIntegral detectRounds)
        (realToFrac detectBias)
        (cDirection detectDir)
        (fromIntegral propRounds)
        (realToFrac propThreshold)
        (cDirection propDir)
        (cDirection mergeDir)
    if r /= c'SUCCESS
      then error $ "quadForestSegmentEdges failed with " ++ (show r)
      else quadForestRefresh forest

quadForestGetSegments :: QuadForest -> IO [ForestSegment]
quadForestGetSegments f =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      targetSize = quadForestSegments f
      allocTargetArray :: IO (Ptr (Ptr C'quad_forest_segment))
      allocTargetArray = mallocArray targetSize
    ptarget <- allocTargetArray
    c'quad_forest_get_segments pforest ptarget
    target <- (peekArray targetSize ptarget)
    mapM forestSegmentFromPtr target

quadForestGetSegmentTrees :: QuadForest -> [ForestSegment] -> IO [QuadTree]
quadForestGetSegmentTrees forest ss = do -- 14 x 29
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (quadForestPtr forest) $ \pforest ->
      withArray (map segmentPtr ss) $ \psegments -> do
        r <- c'quad_forest_get_segment_trees plist pforest psegments
            (fromIntegral $ length ss)
        if r /= c'SUCCESS
           then error $ "Getting segment trees failed with " ++ (show r)
           else createList plist quadTreeFromListItem



-- | Creates an image tree from a list item by casting and converting
edgeChainFromListItem :: Ptr C'list_item -> IO (Ptr C'quad_forest_edge_chain)
edgeChainFromListItem pitem = do
  item <- peek pitem
  return $ castPtr $ c'list_item'data item
  --return $ c'quad_forest_edge_chain'first edgechain

createEdgeChain :: Ptr C'quad_forest_edge_chain -> IO [((Int,Int),(Int,Int))]
createEdgeChain pchain = do
  flist <- allocList
  withForeignPtr flist $ \plist -> do
    r <- c'quad_forest_get_edge_chain pchain plist
    if r /= c'SUCCESS
      then error $ "Getting edge chain failed with " ++ (show r)
      else createList plist lineFromListItem

getPathSniffers :: QuadForest -> IO [((Int,Int),(Int,Int))]
getPathSniffers forest = do
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      r <- c'quad_forest_get_path_sniffers pforest plist
      if r /= c'SUCCESS
        then error $ "Getting path sniffers failed with " ++ (show r)
        else createList plist lineFromListItem

getForestLinks :: QuadForest -> IO [(((Int,Int),(Int,Int)),Double)]
getForestLinks forest = do
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      r <- c'quad_forest_get_links pforest plist
      if r /= c'SUCCESS
        then error $ "Getting links failed with " ++ (show r)
        else createList plist weightedLineFromListItem

{-
IO [ForestEdge]
createEdgeChain nullPtr = return []
createEdgeChain pedge = do
  edge <- peek pedge
  --next <- peek $ p'quad_forest_edge'next pedge
  liftM ((:) (hForestEdge edge)) (createEdgeChain $ c'quad_forest_edge'next edge)
  -}

quadForestGetBoundaries :: QuadForest -> IO [[((Int,Int),(Int,Int))]]
quadForestGetBoundaries forest = do
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    l <- createList (p'quad_forest'edges pforest) edgeChainFromListItem
    mapM createEdgeChain l

forestSegmentNeighbors :: QuadForest -> [ForestSegment] -> IO [ForestSegment]
forestSegmentNeighbors forest ss = do
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (quadForestPtr forest) $ \pforest ->
      withArray (map segmentPtr ss) $ \psegments -> do
        r <- c'quad_forest_get_segment_neighbors plist pforest psegments
            (fromIntegral $ length ss)
        if r /= c'SUCCESS
           then error $ "Getting segment neighbors failed with " ++ (show r)
           else createList plist forestSegmentFromListItem

-- | Creates a bitmask from a collection of segments. The result will be an
--   image with the same dimensions as the minimum covering rectangle of the
--   segment collection. By default those pixels that belong to the listed
--   segments will be white (255) and the rest will be black (0). This behavior
--   can be inverted by giving True as the value of the invert parameter.
quadForestGetSegmentMask :: QuadForest -> Bool -> [ForestSegment] -> IO PixelImage
quadForestGetSegmentMask forest invert ss = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withForeignPtr (quadForestPtr forest) $ \pforest ->
      withArray (map segmentPtr ss) $ \psegments -> do
        r <- c'quad_forest_get_segment_mask pforest pimg psegments
            (fromIntegral $ length ss)
            (cBool invert)
        if r /= c'SUCCESS
           then error $ "Getting segment mask failed with " ++ (show r)
           else ptrToPixelImage fimg

-- | Extract the boundary curve of a segment as a list of lines
quadForestGetSegmentBoundary :: QuadForest -> ForestSegment -> IO [((Int,Int),(Int,Int))]
quadForestGetSegmentBoundary forest segment = do
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      r <- c'quad_forest_get_segment_boundary pforest
          (segmentPtr segment) plist
      if r /= c'SUCCESS
        then error $ "Getting segment boundary failed with " ++ (show r)
        else createList plist lineFromListItem

quadForestHighlightSegments :: QuadForest -> PixelImage -> (Float,Float,Float)
    -> [ForestSegment] -> IO PixelImage
quadForestHighlightSegments forest image (c1,c2,c3) ss =
  withForeignPtr (quadForestPtr forest) $ \pforest ->
    withForeignPtr (imagePtr image) $ \pimage ->
      withArray (map segmentPtr ss) $ \psegments -> do
        let
          color :: [CUChar]
          color = map (fromIntegral.round.(*255)) [c1,c2,c3,0]
        withArray color $ \pcolor -> do
          r <- c'quad_forest_highlight_segments pforest pimage psegments
              (fromIntegral $ length ss) pcolor
          if r /= c'SUCCESS
            then error $ "Highlighting segments failed with " ++ (show r)
            else ptrToPixelImage (imagePtr image)

-- | Draws an image of the forest using the current division and region info.
--   Information from regions or individual trees will be used (based on
--   parameter useRegions) and either the mean intensity of the whole region or
--   the assigned color of the region can be used (based on parameter useColors).
quadForestDrawImage :: Bool -> Bool -> QuadForest -> IO (PixelImage)
quadForestDrawImage useRegions useColors forest = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      r <- c'quad_forest_draw_image
        pforest
        pimg
        (if useRegions then 1 else 0)
        (if useColors then 1 else 0)
      if r /= c'SUCCESS
         then error "Drawing forest image failed"
         else ptrToPixelImage fimg

-- | Draws tree values over an existing image.
--   TODO: maybe would be more flexible to use tree list instead of forest?
quadForestDrawTrees :: Bool -> QuadForest -> PixelImage -> IO PixelImage
quadForestDrawTrees useSegments forest image =
  withForeignPtr (imagePtr image) $ \pimage ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      r <- c'quad_forest_draw_trees pforest pimage (cBool useSegments)
      if r /= c'SUCCESS
        then error $ "Drawing trees failed with " ++ (show r)
        else ptrToPixelImage $ imagePtr image

deep :: NFData a => a -> a
deep a = deepseq a a

instance NFData Statistics where
  rnf stat =
    items stat `deepseq`
    sum1 stat `deepseq`
    sum2 stat `deepseq`
    mean stat `deepseq`
    variance stat `deepseq`
    deviation stat `deepseq` ()

instance NFData Direction where
  rnf dir = dir `seq` ()

instance NFData QuadTree where
  rnf tree =
    quadTreeX tree `deepseq`
    quadTreeY tree `deepseq`
    quadTreeSize tree `deepseq`
    quadTreeLevel tree `deepseq`
    quadTreeStat tree `deepseq`
    quadTreeEdge tree `seq`
    quadTreeChildNW tree `seq`
    quadTreeChildNE tree `seq`
    quadTreeChildSW tree `seq`
    quadTreeChildSE tree `seq` ()

instance NFData ForestSegment where
  rnf segment =
    segmentX segment `deepseq`
    segmentY segment `deepseq`
    segmentW segment `deepseq`
    segmentH segment `deepseq`
    segmentStat segment `deepseq`
    segmentColor segment `deepseq` ()

instance NFData ForestEdge where
  rnf edge =
    edgeDX edge `deepseq`
    edgeDY edge `deepseq`
    edgeMag edge `deepseq`
    edgeAng edge `deepseq`
    edgeMean edge `deepseq`
    edgeDev edge `deepseq`
    edgeFound edge `deepseq`
    edgeDir edge `deepseq` ()

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
  b <- rpar (f a)
  bs <- parMap' f as
  return (b:bs)

mapDeep :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
mapDeep _ [] = []
mapDeep op xs =
  unsafePerformIO $ do
    evaluate $! deep $ runEval $ parMap' op $ xs
