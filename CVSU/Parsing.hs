module CVSU.Parsing
( quadForestParse
, quadForestVisualizeParseResult
) where

import CVSU.Bindings.Parsing
import CVSU.PixelImage
import CVSU.QuadForest

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)

quadForestParse :: QuadForest -> IO QuadForest
quadForestParse forest =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_forest_parse pforest
    if r /= c'SUCCESS
      then error $ "quadForestParse failed with " ++ (show r)
      else quadForestRefresh forest

quadForestVisualizeParseResult :: QuadForest -> PixelImage -> IO PixelImage
quadForestVisualizeParseResult forest =
  withForeignPtr (quadForestPtr forest) $ \pforest ->
    withForeignPtr (imagePtr image) $ \pimage -> do
      r <- c'quad_forest_visualize_parse_result pforest pimage
      if r /= c'SUCCESS
         then error $ "Visualizing parse result failed with " ++ (show r)
         else ptrToPixelImage (imagePTr image)
