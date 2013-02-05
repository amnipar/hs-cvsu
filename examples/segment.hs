module Main where

import CVSU.Types
import CVSU.PixelImage hiding (readPixelImage,writePixelImage)
import CVSU.QuadForest
import CV.Image
import CV.CVSU

import ReadArgs

devSmallerThan t = ((<t).deviation.quadTreeStat)

statSeparation s s1 s2 =
  ((abs $ (mean s1) - (mean s2)) / (max 1 (deviation s1 + deviation s2))) < s

treeSeparation s t1 t2 = statSeparation s (quadTreeStat t1) (quadTreeStat t2)

segmentSeparation s s1 s2 = statSeparation s (segmentStat s1) (segmentStat s2)

main = do
  (sourceFile, targetFile, maxSize, minSize, devthreshold, septhreshold) <- readArgs
  pimg <- readPixelImage sourceFile
  forest <- quadForestCreate pimg maxSize minSize
  sforest <- quadForestSegment (devSmallerThan devthreshold)
        (treeSeparation septhreshold) (segmentSeparation septhreshold) forest
  writePixelImage targetFile =<< quadForestDrawImage True True sforest
