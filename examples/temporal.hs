{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import CVSU.PixelImage
import CVSU.TemporalForest
import CV.CVSU
import CV.CVSU.Rectangle
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
  showImage "temporal" $
      drawLines (1,1,0) 2 (concat bs) $
        drawBoxes (0,0,1) 2 (map segToRect ss) $
      uimage
  waitKey 20
  return ()

main = do
  Just cap <- captureFromCam (0)
  Just f <- getFrame cap
  pimg <- toPixelImage =<< unsafeImageTo8Bit <$> rgbToGray <$> expectFloatRGB f
  tforest <- temporalForestCreate 16 4 3 30 pimg
  win <- makeWindow "temporal"
  runStream_ . sideEffect (handleFrame tforest) . takeS (300) $ streamFromVideo cap
  waitKey 10
  destroyWindow "temporal"
