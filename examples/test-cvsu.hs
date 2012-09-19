{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.PixelImage as CVSU
import CVSU.Edges
import CV.Image
import CV.Pixelwise
import CV.ColourUtils
import CV.Drawing
import CV.ImageOp
import Data.Function
import Data.List
import Data.Ord
import Control.Monad

import Debug.Trace

createFromPixels :: Int -> Int -> [((Int,Int),Float)] -> IO (Image GrayScale D32)
createFromPixels w h ps = do
  i::Image GrayScale D32 <- create (w,h)
  mapM_ (sp i) ps
  return $ stretchHistogram i
  where
        minV = minimum $ map snd ps
        maxV = maximum $ map snd ps
        sp i ((x,y),v) = setPixel (x,y) v i
        --i = imageFromFunction (w,h) (const 0)

drawEdges :: EdgeImage -> Image GrayScale D32 -> Image GrayScale D32
drawEdges eimg i = trace (show maxV) $
  i
  <## [lineOp 1 1 (x,y) (x+8,y) | (Edge x y _ v) <- he] -- ((abs v) / maxV)
  <## [lineOp 1 1 (x,y) (x,y+8) | (Edge x y _ v) <- ve]
  where
        maxV = maximum $ map (abs.value) (he ++ ve)
        he = filter ((>1).abs.value) $ concat $ hedges eimg
        ve = filter ((>1).abs.value) $ concat $ vedges eimg

main = do
  pimg <- readPixelImage "rengas.jpg"
  --withPixelImage pimg $ \i -> do
  eimg <- createEdgeImage 8 8 8 8 8 4 pimg
  ps <- CVSU.getAllPixels pimg
  nimg <- createFromPixels (width pimg) (height pimg) ps
  saveImage "result.png" $ drawEdges eimg nimg
