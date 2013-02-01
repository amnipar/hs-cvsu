{-# LANGUAGE OverlappingInstances, TupleSections, ScopedTypeVariables #-}

module CVSU.QuadForest
( QuadTree(..)
, QuadForest(..)
, ForestSegment(..)
, ForestEdge(..)
, segmentLeft
, segmentTop
, segmentRight
, segmentBottom
, quadForestCreate
, quadForestUpdate
, quadForestRefresh
, withQuadForest
, quadForestGetTree
, quadTreeHasEdge
, quadTreeHasVEdge
, quadTreeHasHEdge
, quadTreeChildStat
, quadTreeNeighborhoodStat
, quadTreeDivide
, quadTreeNeighbors
, quadTreeEdgeResponse
, quadTreeSegmentInit
, quadTreeSegmentUnion
, quadTreeSegmentFind
, quadForestSegment
, quadForestSegmentByDeviation
, quadForestSegmentByOverlap
, quadForestFindEdges
, quadForestSegmentEdges
, quadForestGetSegments
, quadForestGetSegmentMask
, quadForestHighlightSegments
, quadForestDrawImage
, mapDeep
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
-- import CV.Image
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
  , segmentColor :: (Float,Float,Float)
  } deriving Eq

segmentLeft = segmentX
segmentTop = segmentY
segmentRight s = segmentX s + segmentW s
segmentBottom s = segmentY s + segmentH s

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

data QuadTree = EmptyQuadTree |
  QuadTree
  { quadTreePtr :: Ptr C'quad_tree
  , quadTreeX :: Int
  , quadTreeY :: Int
  , quadTreeSize :: Int
  , quadTreeLevel :: Int
  , quadTreeStat :: Statistics
  , quadTreeEdge :: ForestEdge
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

quadForestInit :: PixelImage -> Int -> Int -> IO QuadForest
quadForestInit i maxSize minSize = do
  fforest <- quadForestAlloc
  withForeignPtr fforest $ \pforest ->
    withForeignPtr (imagePtr i) $ \pimage -> do
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
          return $ QuadForest fforest i
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
quadForestCreate :: PixelImage -> Int -> Int -> IO QuadForest
quadForestCreate i maxSize minSize = do
  f <- quadForestInit i maxSize minSize
  quadForestUpdate f

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
quadForestRefresh f =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
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
      (quadForestPtr f)
      (quadForestImage f)
      (fromIntegral r)
      (fromIntegral c)
      (fromIntegral s)
      (fromIntegral smax)
      (fromIntegral smin)
      (fromIntegral dx)
      (fromIntegral dy)
      ts

hForestEdge
  C'quad_forest_edge{
      c'quad_forest_edge'parent = parent,
      c'quad_forest_edge'dx = dx,
      c'quad_forest_edge'dy = dy,
      c'quad_forest_edge'mag = mag,
      c'quad_forest_edge'ang = ang,
      c'quad_forest_edge'mean = mean,
      c'quad_forest_edge'deviation = dev,
      c'quad_forest_edge'has_edge = has_edge,
      c'quad_forest_edge'dir = dir
    } = ForestEdge parent
      (realToFrac dx)
      (realToFrac dy)
      (realToFrac mag)
      (realToFrac ang)
      (realToFrac mean)
      (realToFrac dev)
      (hBool has_edge)
      (hDirection dir)

quadTreeFromPtr :: Ptr C'quad_tree -> IO QuadTree
quadTreeFromPtr ptree
  | ptree == nullPtr = return EmptyQuadTree
  | otherwise        = do
    C'quad_tree{
      c'quad_tree'x = x,
      c'quad_tree'y = y,
      c'quad_tree'size = s,
      c'quad_tree'level = l,
      c'quad_tree'stat = stat,
      c'quad_tree'edge = edge,
      c'quad_tree'nw = nw,
      c'quad_tree'ne = ne,
      c'quad_tree'sw = sw,
      c'quad_tree'se = se
    } <- peek ptree
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
      tnw tne tsw tse

withQuadForest :: QuadForest -> (QuadForest -> IO a) -> IO a
withQuadForest f op =
  withForeignPtr (quadForestPtr f) $ \_ -> do
    r <- op f
    touchForeignPtr (quadForestPtr f)
    return $! r

-- | Extract the given root tree from the forest
quadForestGetTree :: QuadForest -> (Int,Int) -> IO QuadTree
quadForestGetTree f (x,y) =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    forest <- peek pforest
    root <- peek $ advancePtr (c'quad_forest'roots forest) (y * (quadForestCols f) + x)
    quadTreeFromPtr root

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

quadTreeEdgeResponse :: QuadForest -> QuadTree -> IO Double
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
            return $ sqrt $ (realToFrac rdx)**2 + (realToFrac rdy)**2


-- | Divides the tree in four equal parts in quad-tree fashion. Causes side
--   effects as the original tree will have four child trees after this
--   operation.
quadTreeDivide :: QuadForest -> QuadTree -> IO [QuadTree]
quadTreeDivide f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    r <- c'quad_tree_divide pforest (quadTreePtr t)
    if r /= c'SUCCESS
      then error $ "Divide tree failed with " ++ (show r)
      else do
        ct <- peek (quadTreePtr t)
        mapM quadTreeFromPtr
          [ c'quad_tree'nw ct
          , c'quad_tree'ne ct
          , c'quad_tree'sw ct
          , c'quad_tree'se ct ]

-- | Creates an image tree from a list item by casting and converting
quadTreeFromListItem :: Ptr C'list_item -> IO QuadTree
quadTreeFromListItem pitem = do
  item <- peek pitem
  ptree <- peek $ castPtr $ c'list_item'data item
  quadTreeFromPtr $ castPtr ptree

-- | Finds the current neighbors of the given tree.
quadTreeNeighbors :: QuadTree -> IO [QuadTree]
quadTreeNeighbors t = do
  ct <- peek $ quadTreePtr t
  n <- quadTreeFromPtr $ c'quad_tree'n ct
  e <- quadTreeFromPtr $ c'quad_tree'e ct
  s <- quadTreeFromPtr $ c'quad_tree's ct
  w <- quadTreeFromPtr $ c'quad_tree'w ct
  return $ filter (/=EmptyQuadTree) [n, e, s, w]

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
quadTreeSegmentUnion :: (QuadTree,QuadTree) -> IO (QuadTree,QuadTree)
quadTreeSegmentUnion (t1,t2) = do
  c'quad_tree_segment_union (quadTreePtr t1) (quadTreePtr t2)
  t1' <- quadTreeFromPtr $ quadTreePtr t1
  t2' <- quadTreeFromPtr $ quadTreePtr t2
  return $! (t1,t2)

-- | Find part of the Union-Find disjunctive set algorithm. Finds the pointer
--   of the parent tree, caches it in the c structure, and creates an Integer
--   from the pointer. May cause side effects as the parent pointer is
--   cached to shorten the search paths for better efficiency.
quadTreeSegmentFind :: QuadTree -> IO (Integer)
quadTreeSegmentFind t = do
  tid <- c'quad_tree_segment_get (quadTreePtr t)
  return $ fromIntegral tid

-- | Segments a forest by dividing trees until they are consistent by the
--   given measure, and by grouping neighboring trees, that are deemed equal in
--   terms of the provided equivalence operation, by using equivalence classes
--   implemented for image trees using Union-Find algorithm. The result is a
--   segmented forest where trees belonging to the same region have the same
--   classId. Note: the treeDivide and treeClassUnion operations cause side
--   effects, so this whole operation must be performed in full to leave the
--   tree list in consistent state, and in the current form, the algorithm is
--   not safe to parallelize (though in future it probably will be). Also the
--   resulting Haskell data structure is not persistent (may be later).
quadForestSegment :: (QuadTree -> Bool) -> (QuadTree -> QuadTree -> Bool)
    -> QuadForest -> IO QuadForest
quadForestSegment isConsistent isEq f = do
  ts <- segment $ quadForestTrees f
  ts `seq` quadForestRefresh f
  where
    minSize = quadForestMinSize f
    -- find all neighbors and create a union with those that are equivalent and
    -- consistent according to given measures
    --unionWithNeighbors :: (ForestValue a) => ImageTree a -> IO [(ImageTree a, ImageTree a)]
    unionWithNeighbors t = do
      ns :: [QuadTree] <- quadTreeNeighbors t
      mapM_ (quadTreeSegmentUnion.(t,)) $ filter (\n -> (isConsistent n) && (isEq t n)) ns
    -- go through all trees; add consistent ones to segments, divide in four
    -- the inconsistent ones
    --segment :: (ForestValue a) => [ImageTree a] -> IO ()
    segment [] = return ()
    segment (t:ts)
      | isConsistent t = do
        rs <- unionWithNeighbors t
        rs `seq` segment ts
      | quadTreeSize t > minSize = do
        cs <- quadTreeDivide f t
        cs `seq` segment (ts ++ cs)
      | otherwise = segment ts

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

quadForestSegmentEdges :: Int -> Double -> Direction -> Int -> Double
      -> Direction -> Direction -> QuadForest -> IO QuadForest
quadForestSegmentEdges detectRounds detectBias detectDir propRounds
      propThreshold propDir mergeDir forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_segment_horizontal_edges pforest
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
      makeC = (/255).realToFrac
      toColor c1 c2 c3 = (makeC c1, makeC c2, makeC c3)
      readSegment :: Ptr C'quad_forest_segment -> IO (ForestSegment)
      readSegment pregion = do
        (C'quad_forest_segment p _ x1 y1 x2 y2 stat c1 c2 c3) <- peek pregion
        return $ ForestSegment
          p
          (fromIntegral x1)
          (fromIntegral y1)
          (fromIntegral $ x2-x1)
          (fromIntegral $ y2-y1)
          (hStatistics stat)
          (toColor c1 c2 c3)
    ptarget <- allocTargetArray
    c'quad_forest_get_segments pforest ptarget
    target <- (peekArray targetSize ptarget)
    mapM readSegment target

-- | Creates a bitmask from a collection of segments. The result will be an
--   image with the same dimensions as the minimum covering rectangle of the
--   segment collection. By default those pixels that belong to the listed
--   segments will be white (255) and the rest will be black (0). This behavior
--   can be inverted by giving True as the value of the invert parameter.
quadForestGetSegmentMask :: QuadForest -> Bool -> [ForestSegment] -> IO PixelImage
quadForestGetSegmentMask forest invert ss = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withForeignPtr (quadForestPtr forest) $ \pforest -> do
      withArray (map segmentPtr ss) $ \psegments -> do
        r <- c'quad_forest_get_segment_mask pforest pimg psegments
            (fromIntegral $ length ss)
            (cBool invert)
        if r /= c'SUCCESS
           then error $ "Getting segment mask failed with " ++ (show r)
           else ptrToPixelImage fimg

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

deep :: NFData a => a -> a
deep a = deepseq a a

instance NFData QuadTree where
  rnf tree =
    quadTreeChildNW tree `seq`
    quadTreeChildNE tree `seq`
    quadTreeChildSW tree `seq`
    quadTreeChildSE tree `seq` ()

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
