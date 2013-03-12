module CVSU.TemporalForest
( TemporalForest(..)
, temporalForestCreate
, temporalForestUpdate
, temporalForestVisualize
, temporalForestGetSegments
, temporalForestGetSegmentBoundary
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.QuadForest
import CVSU.Bindings.TemporalForest
import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest
import CVSU.List

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Array
import Foreign.Concurrent
import Control.Monad

data TemporalForest =
  TemporalForest
  { temporalForestPtr :: !(ForeignPtr C'temporal_forest)
  , temporalForestRows :: Int
  , temporalForestCols :: Int
  , temporalForestMaxSize :: Int
  , temporalForestMinSize :: Int
  , temporalForestDX :: Int
  , temporalForestDY :: Int
  }

temporalForestAlloc :: IO (ForeignPtr C'temporal_forest)
temporalForestAlloc = do
  ptr <- c'temporal_forest_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'temporal_forest_free ptr)
    else error "Memory allocation failed in temporalForestAlloc"

temporalForestCreate :: Int -> Int -> Int -> Int -> PixelImage -> IO TemporalForest
temporalForestCreate maxSize minSize frameCount historyCount image = do
  fforest <- temporalForestAlloc
  withForeignPtr fforest $ \pforest ->
    withForeignPtr (imagePtr image) $ \pimage -> do
      r <- c'temporal_forest_create pforest pimage
          (fromIntegral maxSize)
          (fromIntegral minSize)
          (fromIntegral frameCount)
          (fromIntegral historyCount)
      if r /= c'SUCCESS
        then error $ "Temporal forest create failed with " ++ (show r)
        else do
          C'temporal_forest{
            c'temporal_forest'rows = r,
            c'temporal_forest'cols = c,
            c'temporal_forest'tree_max_size = smax,
            c'temporal_forest'tree_min_size = smin,
            c'temporal_forest'dx = dx,
            c'temporal_forest'dy = dy
          } <- peek pforest
          return $ TemporalForest fforest
              (fromIntegral r)
              (fromIntegral c)
              (fromIntegral smax)
              (fromIntegral smin)
              (fromIntegral dx)
              (fromIntegral dy)

temporalForestUpdate :: TemporalForest -> PixelImage -> IO TemporalForest
temporalForestUpdate forest image =
  withForeignPtr (temporalForestPtr forest) $ \pforest ->
    withForeignPtr (imagePtr image) $ \pimage -> do
      r <- c'temporal_forest_update pforest pimage
      if r /= c'SUCCESS
        then error $ "Temporal forest update failed with " ++ (show r)
        else return forest

temporalForestVisualize :: TemporalForest -> IO PixelImage
temporalForestVisualize forest =
  withForeignPtr (temporalForestPtr forest) $ \pforest -> do
    r <- c'temporal_forest_visualize pforest
    if r /= c'SUCCESS
      then error $ "Temporal forest visualize failed with " ++ (show r)
      else do
        let pimage = p'temporal_forest'visual pforest
        fimage <- newForeignPtr pimage ((\p -> return ()) pimage)
        ptrToPixelImage fimage

temporalForestGetSegments :: TemporalForest -> IO [ForestSegment]
temporalForestGetSegments forest =
  withForeignPtr (temporalForestPtr forest) $ \pforest -> do
    targetSize <- liftM fromIntegral $ c'temporal_forest_segment_count pforest
    let
      allocTargetArray :: Int -> IO (Ptr (Ptr C'quad_forest_segment))
      allocTargetArray size = mallocArray size
    ptarget <- allocTargetArray targetSize
    c'temporal_forest_get_segments pforest ptarget
    target <- (peekArray targetSize ptarget)
    mapM forestSegmentFromPtr target

temporalForestGetSegmentBoundary :: TemporalForest -> ForestSegment -> IO [((Int,Int),(Int,Int))]
temporalForestGetSegmentBoundary forest segment = do
  flist <- allocList
  withForeignPtr flist $ \plist ->
    withForeignPtr (temporalForestPtr forest) $ \pforest -> do
      r <- c'temporal_forest_get_segment_boundary pforest
          (segmentPtr segment) plist
      if r /= c'SUCCESS
        then error $ "Getting segment boundary failed with " ++ (show r)
        else createList plist lineFromListItem
