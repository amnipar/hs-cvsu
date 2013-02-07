module Main where

import CVSU.PixelImage hiding (readPixelImage,writePixelImage)
import CVSU.QuadForest
import CV.CVSU
import ReadArgs

-- Segments an image using c implementation of overlap measures for consistency
-- and similarity. Works quite well for regions with high variation and texture,
-- but tends to oversegment. Behavior can be adjusted somewhat with the
-- parameters.
--
-- Parameters:
-- maxSize:     maximum (initial) size for quad forest trees
-- minSize:     minimum size for trees; trees are not divided beyond this size
-- alpha:       deviation multiplier used for estimating intensity ranges in regions
--              (min = mean - alpha * deviation, max = mean + alpha * deviation)
-- treeOverlap: the required range overlap (intersection / union) for trees
-- segOverlap:  the required range overlap (intersection / union) for segments
--
-- Overlap 1 means perfect overlap, 0 means the ranges are completely separate.
-- Value 0.5 means that range union is twice the size of intersection, which
-- works quite well in many situations. Also 0.4, 0.6, 0.7 can be tried.
-- Alpha values 2, 2.5, 3 seem to be ok.
--
-- Example: overlapsegment 16 4 3 0.5 0.5

main = do
  (maxSize, minSize, alpha, treeOverlap, segOverlap, sourceFile, targetFile) <- readArgs
  readPixelImage sourceFile >>= quadForestCreate maxSize minSize >>=
    quadForestSegmentByOverlap alpha treeOverlap segOverlap >>=
    quadForestDrawImage True True >>=
    writePixelImage targetFile
