module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest

import CV.Image
import CV.CVSU
import CV.CVSU.Drawing

import ReadArgs
import Control.Applicative
import System.Directory
import System.FilePath
import System.IO.Unsafe

consistent f t = unsafePerformIO $ do
  ces <- quadTreeChildEdgeResponse f t
  return $ null $ filter (>1) (ds ces)
  where
    ds cs = map (\x -> (abs $ x - meanm) / devm) ms
      where
        ms = map (\(dx,dy) -> sqrt $ dx**2 + dy**2) cs
        meanm = (sum ms) / 4
        devm = sqrt $ min 1 ((sum $ map (**2) ms) / 4 - meanm**2)

getFiles :: FilePath -> IO [FilePath]
getFiles p =
  map (snd.splitFileName) <$> filter (\f -> takeExtension f == ".tif") <$> getDirectoryContents p

edges sourceFile = do
  print sourceFile
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ unsafeImageTo8Bit $ img
  forest <- quadForestCreate 8 4 pimg
  sforest <- quadForestSegmentEdges 3 0.5 DirN4 2 1 DirN4 DirN4 forest
  saveImage ("edges/" ++ sourceFile ++ ".png") $
    drawEdges False (0,1,1) 2 (quadForestTrees sforest) $ grayToRGB img

-- Calculates edge responses with integral images, then segments edges using
-- propagation algorithm and draws the detected edges and optionally also the
-- edge responses.
--
-- Parameters:
-- maxSize:  maximum (initial) size for quad forest trees
-- minSize:  minimum size for trees; trees are not divided beyond this size
-- mode:     edge detection mode (m = magnitude, h = horizontal, v = vertical)
-- drounds:  number of propagation rounds performed in detection phase
-- bias:     bias value used in propagation
-- prounds:  number of propagation rounds in edge connection phase
-- pthresh:  threshold value use in edge connection phase
-- drawResp: draw edge response in addition to detected edges
--
-- Example: edges 8 4 h 3 0.8 1 1 False source.png target.png

main = do
  (maxSize, minSize, sourceFile, targetFile) <- readArgs
  img <- expectFloatGrey =<< readFromFile sourceFile
  pimg <- toPixelImage $ unsafeImageTo8Bit $ img
  forest <- quadForestCreate maxSize minSize pimg
  _ <- divideUntilConsistent (consistent forest) forest $ quadForestTrees forest
  cforest <- quadForestRefresh forest
  let ctrees = quadForestGetTopLevelTrees cforest
  rs <- mapM (quadTreeEdgeResponse cforest) ctrees
  print $ length rs
  eforest <- quadForestRefresh cforest
  let etrees = quadForestGetTopLevelTrees eforest
  print $ map (edgeMag.quadTreeEdge) etrees
  saveImage targetFile $ drawTreeValues (realToFrac.edgeMag.quadTreeEdge) etrees $ grayToRGB img
