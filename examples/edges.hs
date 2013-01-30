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

drawHEdges :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawHEdges img ts =
  rimg
  <## [rectOp (toColor v) (-1) r | (r,v) <- map toRect ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toHLine $ filter quadTreeHasHEdge ts]
  where
    rimg = grayToRGB img
    toColor :: Float -> (Float,Float,Float)
    toColor v = (v/maxV,v/maxV,v/maxV)
    maxV = realToFrac $ maximum $ map (edgeDY.quadTreeEdge) ts
    toRect (QuadTree _ x y s _ _ e _ _ _ _) =
      (mkRectangle (x,y) (s,s), realToFrac $ edgeDY e)
    toHLine (QuadTree _ x y s _ _ _ _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))

drawEdges :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawEdges img ts =
  rimg
  <## [rectOp (toColor v) (-1) r | (r,v) <- map toRect ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toHLine $ filter quadTreeHasEdge ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toVLine $ filter quadTreeHasEdge ts]
  where
    rimg = grayToRGB img
    toColor :: Float -> (Float,Float,Float)
    toColor v = (v/maxV,v/maxV,v/maxV)
    maxV = realToFrac $ maximum $ map (edgeMag.quadTreeEdge) ts
    toRect (QuadTree _ x y s _ _ e _ _ _ _) =
      (mkRectangle (x,y) (s,s), realToFrac $ edgeMag e)
    toHLine (QuadTree _ x y s _ _ _ _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))
    toVLine (QuadTree _ x y s _ _ _ _ _ _ _) = ((x+s`div`2,y),(x+s`div`2,y+s))

main = do
  (sourceFile, targetFile, size, minSize) <- readArgs
  img <- readFromFile sourceFile
  pimg <- fromCVImage $ unsafeImageTo8Bit img
  forest <- quadForestCreate pimg size minSize
  withQuadForest forest $ \f -> do
    --ef <- quadForestFindEdges 4 1 f
    ef <- quadForestFindHorizontalEdges 4 0.5 f
    --saveImage targetFile $ drawEdges img $ quadForestTrees ef
    saveImage targetFile $ drawHEdges img $ quadForestTrees ef
