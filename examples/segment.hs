module Main where

import CVSU.Types
import CVSU.PixelImage hiding (readPixelImage,writePixelImage)
import CVSU.QuadForest
import CV.Image
import CV.CVSU

import ReadArgs

-- Use deviation to decide, if trees are consistent. If tree deviation is
-- smaller than the given threshold, it is deemed consistent. Values like
-- 1 and 2 seem to work well for flat regions with low variation.
devST t = ((<t).deviation.quadTreeStat)

-- Uses the distance between means relative to deviation as a measure of
-- separation between region statistics. Denominator must be at least 1. If
-- the separation is smaller than the given threshold, the regions are deemed
-- similar. Threshold value 1 seems to work well.
statSeparation s s1 s2 =
  ((abs $ (mean s1) - (mean s2)) / (max 1 (deviation s1 + deviation s2))) < s

-- Separation between tree statistics.
treeSep s t1 t2 = statSeparation s (quadTreeStat t1) (quadTreeStat t2)

-- Separation between segment statistics.
segSep s s1 s2 = statSeparation s (segmentStat s1) (segmentStat s2)

-- Segments an image using the generic haskell implementation of quad forest
-- segmentation, where trees are first divided using a consistency measure and
-- minimum tree size as a guide, then neighboring trees are merged using a
-- similarity measure, and finally neighboring segments are merged using
-- another similarity measure.
--
-- Parameters:
-- maxSize:   maximum (initial) size for quad forest trees
-- minSize:   minimum size for trees; trees are not divided beyond this size
-- devthresh: deviation threshold for determining consistency
-- septhresh: separation threshold for determining similarity
--
-- Note: inconsistent trees will stay black, this basic method works well only
-- for images with low-variation regions. High-variation regions require
-- selecting larger devthreshold parameter, but this will risk merging regions
-- with fuzzy borders. Look at overlapsegment.hs for a more robust method, or
-- write better consistency and similarity measures.
--
-- Example: segment 16 4 2 1 triangle.png target.png

main = do
  (maxSize, minSize, devthresh, septhresh, sourceFile, targetFile) <- readArgs
  readPixelImage sourceFile >>= quadForestCreate maxSize minSize >>=
    quadForestSegment (devST devthresh) (treeSep septhresh) (segSep septhresh) >>=
      quadForestDrawImage True True >>=
        writePixelImage targetFile
