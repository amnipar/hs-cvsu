{-# LANGUAGE OverlappingInstances, TupleSections, ScopedTypeVariables #-}

module CVSU.QuadForest
( QuadForest(..)
, ForestSegment(..)
, forestSegmentFromPtr
, forestSegmentUnion
, forestSegmentNeighbors
, ForestEdge(..)
, segmentLeft
, segmentTop
, segmentRight
, segmentBottom
, withQuadForest
, quadForestCreate
, quadForestUpdate
, quadForestRefresh
, quadForestRefreshSegments
, quadForestGetTopLevelTrees
, quadForestGetTree
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
