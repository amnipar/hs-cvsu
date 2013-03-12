{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import CVSU.PixelImage
import CVSU.TemporalForest
import CV.CVSU
import CV.CVSU.Drawing
import CV.Image
import CV.Video
import CV.HighGUI
import Utils.Stream
import Control.Concurrent
import Control.Monad
import Control.Applicative

handleFrame tforest image = do
  pimg <- toPixelImage =<< unsafeImageTo8Bit <$> rgbToGray <$> expectFloatRGB image
  uforest <- temporalForestUpdate tforest pimg
  uimg <- temporalForestVisualize uforest
  ss <- temporalForestGetSegments uforest
  bs <- mapM (temporalForestGetSegmentBoundary uforest) ss
  uimage <- liftM unsafeImageTo32F <$> expectByteRGB =<< fromPixelImage uimg
  showImage "test" $ drawLines (0,1,1) 2 (concat bs) $ uimage
  waitKey 20
  print "frame handled"

main = do
  print "finding capture"
  Just cap <- captureFromCam (0)
  print "capture acquired"
  Just f <- getFrame cap
  let (w,h) = getSize f
  print $ "image size " ++ (show (w,h))
  pimg <- toPixelImage =<< unsafeImageTo8Bit <$> rgbToGray <$> expectFloatRGB f
  tforest <- temporalForestCreate 16 4 3 30 pimg
  win <- makeWindow "test"
  runStream_ . sideEffect (handleFrame tforest) . takeS (300) $ streamFromVideo cap
  waitKey 10
  destroyWindow "test"
