{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage as CVSU
import CVSU.ImageTree as T
import CVSU.Edges as E
import CV.Image
import CV.Pixelwise
import CV.ColourUtils
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle
import Data.Function
import Data.List
import Data.Ord
import Control.Monad

import Debug.Trace

createFromPixels :: Int -> Int -> [((Int,Int),Float)] -> IO (Image GrayScale D32)
createFromPixels w h ps = do
  i <- create (w,h)
  mapM_ (sp i) ps
  return $ stretchHistogram i
  where
        minV = minimum $ map snd ps
        maxV = maximum $ map snd ps
        sp i ((x,y),v) = setPixel (x,y) v i
        --i = imageFromFunction (w,h) (const 0)

drawEdges :: EdgeImage -> Image GrayScale D32 -> Image GrayScale D32
drawEdges eimg i =
  i
  <## [lineOp 1 1 (x,y) (x+8,y) | (Edge x y _ v) <- he] -- ((abs v) / maxV)
  <## [lineOp 1 1 (x,y) (x,y+8) | (Edge x y _ v) <- ve]
  where
        maxV = maximum $ map (abs . E.value) (he ++ ve)
        he = filter ((>1) . abs . E.value) $ concat $ hedges eimg
        ve = filter ((>1) . abs . E.value) $ concat $ vedges eimg

drawBlocks :: ImageForest Stat -> Image GrayScale D32 -> Image GrayScale D32
drawBlocks (ImageForest ptr _ _ _ _ ts) i =
  i
  <## [rectOp 1 1 r | r <- map toRect $ filter ((>10) . statDev . T.value) $ map block ts]
  where
    toRect (ImageBlock n e s w _) = mkRectangle (round w, round n) (round (s-n), round (e-w))

main = do
  pimg <- readPixelImage "smallLena.jpg"
  --withPixelImage pimg $ \i -> do
  eimg <- createEdgeImage 8 8 8 8 8 4 pimg
  forest <- createForest pimg (8,8)
  ps <- CVSU.getAllPixels pimg
  nimg <- createFromPixels (width pimg) (height pimg) ps
  saveImage "result.png" $ drawBlocks forest $ drawEdges eimg nimg
