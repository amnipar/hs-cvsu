{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest
import CVSU.OpenCV

import CV.Image
import CV.Filters
import CV.Matrix as M
import CV.Drawing
import CV.ImageOp
import CV.Pixelwise
import Utils.Rectangle

import ReadArgs
import Control.Monad
import System.IO.Unsafe
import GHC.Float
import Foreign.Ptr

fromCVImage :: Image GrayScale D8 -> IO (PixelImage)
fromCVImage img = do
  saveImage "temp.png" img
  withGenImage img $ \pimg ->
    fromIplImage FormatGrey (castPtr pimg)

drawVEdges :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawVEdges img ts =
  rimg
  <## [rectOp (toColor v) (-1) r | (r,v) <- map toRect ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toHLine $ filter quadTreeHEdge ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toVLine $ filter quadTreeVEdge ts]
  where
    rimg = grayToRGB img
    toColor :: Float -> (Float,Float,Float)
    toColor v = (v/maxV,v/maxV,v/maxV)
    maxV = realToFrac $ maximum $ map quadTreeDY ts
    toRect (QuadTree _ x y s _ _ dx dy _ _ _ _ _ _) = 
      (mkRectangle (x,y) (s,s), realToFrac $ sqrt $ dx**2 + dy**2)
    toHLine (QuadTree _ x y s _ _ _ _ _ _ _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))
    toVLine (QuadTree _ x y s _ _ _ _ _ _ _ _ _ _) = ((x+s`div`2,y),(x+s`div`2,y+s))

main = do
  (sourceFile, targetFile, size, minSize) <- readArgs
  print "1"
  img <- readFromFile sourceFile
  print "2"
  pimg <- fromCVImage $ unsafeImageTo8Bit img
  print "3"
  forest <- quadForestCreate pimg size minSize
  print "4"
  withQuadForest forest $ \f -> do
    ef <- quadForestGetHorizontalEdges f
    print "5"
    saveImage targetFile $ drawVEdges img $ quadForestTrees ef
