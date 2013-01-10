module Main where

import CVSU.PixelImage as P
import CVSU.Integral
import CVSU.ConnectedComponents
import CVSU.OpenCV

import CV.Image
import CV.Morphology
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle

import ReadArgs
import Foreign.Ptr
import Control.Monad
import System.IO.Unsafe
import Debug.Trace

fromCVImage :: Image GrayScale D8 -> IO (PixelImage)
fromCVImage img = do
  saveImage "temp.png" img
  withGenImage img $ \pimg ->
    fromIplImage (castPtr pimg)

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
  timg <- integralThresholdFeng True True 3 size 3 int
  t <- toCVImageG timg
  ci <- fromCVImage $ c t
  comp <- createConnectedComponents ci
  cimg <- drawConnectedComponents comp
  img <- toCVImage cimg
  saveImage targetFile $ drawComponentRects img comp
  saveImage "feng.png" =<< toCVImage timg
  saveImage "close.png" $ c t
  where
    c t = unsafeImageTo8Bit $ erode se5 2 $ dilate se5 1 $ unsafeImageTo32F t
    se5 = structuringElement (5,5) (2,2) RectShape
    se7 = structuringElement (7,7) (3,3) RectShape
