module Main where

import CVSU.PixelImage as P
import CVSU.Integral
import CVSU.ConnectedComponents
import CVSU.OpenCV

import CV.Image
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle

import ReadArgs
import Foreign.Ptr
import Control.Monad
import System.IO.Unsafe
import Debug.Trace

toCVImageG :: PixelImage -> IO (Image GrayScale D8)
toCVImageG img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

toCVImage :: PixelImage -> IO (Image RGB D8)
toCVImage img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

drawComponentRects :: Image RGB D8 -> ConnectedComponents -> Image RGB D32
drawComponentRects img comp =
  rimg <## [rectOp c 1 (mkRectangle (x,y) (w,h)) | (ConnectedComponent x y w h c) <- cs]
  where
    cs = components comp
    rimg = unsafeImageTo32F img

main = do
  (sourceFile, targetFile, size) <- readArgs
  pimg <- readPixelImage sourceFile
  int <- createIntegralImage pimg
  timg <- integralThresholdFeng size 0.5 int
  comp <- createConnectedComponents timg
  cimg <- drawConnectedComponents comp
  img <- toCVImage cimg
  saveImage targetFile $ drawComponentRects img comp
  saveImage "feng.png" =<< toCVImage timg
