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
  (maxSize, minSize, drounds, bias, minLength, sourceFile, targetFile) <- readArgs
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ unsafeImageTo8Bit $ img
  forest <- quadForestCreate maxSize minSize pimg
  sforest <- quadForestFindBoundaries drounds bias minLength forest
  bs <- quadForestGetBoundaries sforest
  ps <- getPathSniffers sforest
  saveImage targetFile $ 
    drawLines (0,0,1) 1 ps $
    drawLines (0,1,1) 2 (concat bs) $ grayToRGB img
  --saveImage targetFile $ drawBoundaries True (0,1,1) 2 (quadForestTrees sforest) $ grayToRGB img
