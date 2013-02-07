module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest

import CV.Image
import CV.CVSU
import CV.CVSU.Drawing

import ReadArgs

-- Calculates edge responses with integral images, then segments edges using
-- propagation algorithm and draws the detected edges and optionally also the
-- edge responses.
--
-- Parameters:
-- maxSize:  maximum (initial) size for quad forest trees
-- minSize:  minimum size for trees; trees are not divided beyond this size
-- mode:     edge detection mode (m = magnitude, h = horizontal, v = vertical)
-- drounds:  number of propagation rounds performed in detection phase
-- bias:     bias value used in propagation
-- prounds:  number of propagation rounds in edge connection phase
-- pthresh:  threshold value use in edge connection phase
-- drawResp: draw edge response in addition to detected edges
--
-- Example: edges 8 4 h 3 0.8 1 1 False source.png target.png

main = do
  (maxSize, minSize, mode, drounds, bias, crounds, cthresh, drawResp, sourceFile, targetFile) <- readArgs
  -- determine propagation directions and drawing function based on mode
  (detectDir, propagateDir, drawFun) <- case mode of
      "m" -> return (DirN4, DirN4, drawEdges)
      "h" -> return (DirH,  DirV,  drawHEdges)
      "v" -> return (DirV,  DirH,  drawVEdges)
      _   -> error "mode must be m (magnitude) or h (horizontal) or v (vertical)"
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ unsafeImageTo8Bit $ img
  forest <- quadForestCreate maxSize minSize pimg
  sforest <- quadForestSegmentEdges drounds bias detectDir crounds cthresh propagateDir DirN4 forest
  saveImage targetFile $ drawFun drawResp (0,1,1) 2 (quadForestTrees sforest) $ grayToRGB img
