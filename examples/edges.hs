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

toCVImage :: PixelImage -> IO (Image RGB D8)
toCVImage img = creatingImage $ toBareImage $ toIplImage FormatRGB img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

toCVImageG :: PixelImage -> IO (Image GrayScale D8)
toCVImageG img = creatingImage $ toBareImage $ toIplImage FormatGrey img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

drawEdges :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawEdges img ts =
  rimg
  <## [rectOp (toColor v) (-1) r | (r,v) <- map toRect ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toLine $ filter quadTreeHasEdge ts]
  where
    rimg = grayToRGB img
    toColor :: Float -> (Float,Float,Float)
    toColor v = (v/maxV,v/maxV,v/maxV)
    maxV = realToFrac $ maximum $ map (edgeMag.quadTreeEdge) ts
    toRect (QuadTree _ x y s _ _ e _ _ _ _) =
      (mkRectangle (x,y) (s,s), realToFrac $ edgeMag e)
    toLine (QuadTree _ x y s _ _ e _ _ _ _) =
      ((x+d-dx,y+d-dy),(x+d+dx,y+d+dy))
      where
        dx = round $ (edgeDY e / m) * fromIntegral d
        dy = round $ (edgeDX e / m) * fromIntegral d
        d = s `div` 2
        m = max (edgeDX e) (edgeDY e)

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
    toHLine (QuadTree _ x y s _ _ e _ _ _ _) = ((x,y+s`div`2),(x+s,y+s`div`2))

drawVEdges :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawVEdges img ts =
  rimg
  <## [rectOp (toColor v) (-1) r | (r,v) <- map toRect ts]
  <## [lineOp (0,1,1) 2 (x1,y1) (x2,y2) | ((x1,y1),(x2,y2)) <- map toVLine $ filter quadTreeHasVEdge ts]
  where
    rimg = grayToRGB img
    toColor :: Float -> (Float,Float,Float)
    toColor v = (v/maxV,v/maxV,v/maxV)
    maxV = realToFrac $ maximum $ map (edgeDX.quadTreeEdge) ts
    toRect (QuadTree _ x y s _ _ e _ _ _ _) =
      (mkRectangle (x,y) (s,s), realToFrac $ edgeDX e)
    toVLine (QuadTree _ x y s _ _ _ _ _ _ _) = ((x+s`div`2,y),(x+s`div`2,y+s))

main = do
  (sourceFile, targetFile, mode, size, minSize) <- readArgs
  (find,draw,bias) <- case mode of
      "m" -> return (quadForestFindEdges, drawEdges,1.5)
      "h" -> return (quadForestFindHorizontalEdges, drawHEdges,0.5)
      "v" -> return (quadForestFindVerticalEdges, drawVEdges,0.5)
  img <- readFromFile sourceFile
  pimg <- fromCVImage $ unsafeImageTo8Bit img
  forest <- quadForestCreate pimg size minSize
  withQuadForest forest $ \f -> do
    --ef <- quadForestFindEdges 4 1 f
    --ef <- find 4 bias f
    sf <- quadForestSegmentHorizontalEdges 4 0.5 True True f
    segments <- quadForestGetSegments sf
    let
      bySize (ForestSegment _ _ _ w h _ _) = w > 8 && w < 380 && h > 8 && h < 170
    --rimg <- quadForestGetSegmentMask sf False $ filter bySize segments
    print $ length $ filter bySize segments
    saveImage "segmented.png" =<< toCVImage =<< quadForestDrawImage True True forest -- =<< toCVImageG rimg -- 
    --saveImage targetFile $ drawEdges img $ quadForestTrees ef
    saveImage targetFile $ draw img $ quadForestTrees sf
