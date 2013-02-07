module Main where

import CVSU.PixelImage hiding (readPixelImage,writePixelImage)
import CVSU.Integral
import CVSU.ConnectedComponents

import CV.Image
import CV.CVSU

import ReadArgs

-- Segments an image using Feng's improved Sauvola thresholding and connected
-- components analysis.
--
-- Parameters:
-- radius:     size of the neighborhood for determining level of variation
-- multiplier: radius is multiplied with this to determine the size of the
--             larger neighborhood for determining variation threshold
-- invert:     by default set values greater than threshold to white and
--             smaller values to black; setting this to True inverts this
-- estMin:     by default detects the minimum value in the neighborhood by
--             looking at all pixels; if this is True, the minimum value is
--             estimated as mean - alpha * deviation
-- alpha:      deviation multiplier used for estimating minimum value
--
-- Example: threshold 5 3 False True 3 source.png target.png

main = do
  (radius, multiplier, invert, estMin, alpha, sourceFile, targetFile) <- readArgs
  readPixelImage sourceFile >>= createIntegralImage >>=
    integralThresholdFeng invert estMin alpha radius multiplier >>=
    createConnectedComponents >>= drawConnectedComponents >>=
    writePixelImage targetFile
