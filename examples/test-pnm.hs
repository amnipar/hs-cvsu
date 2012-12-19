module Main where

import CVSU.PixelImage
import ReadArgs

main = do
  (sourceFile, targetFile, ascii) <- readArgs
  ppm <- readPNMPixelImage sourceFile
  writePNMPixelImage targetFile (ascii == "1") ppm
