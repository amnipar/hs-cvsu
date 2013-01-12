{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest
import CVSU.OpenCV

import CV.Image
import CV.Filters
import CV.Matrix as M
import CV.Drawing
import CV.ImageOp
import CV.Pixelwise
import Utils.Rectangle

import ReadArgs
import Control.Monad
import System.IO.Unsafe
import GHC.Float
import Foreign.Ptr

fi = fromIntegral

g :: Float -> (Int,Int) -> Float
g s (x,y) = (1 / (2 * pi * s**2)) * exp(-(((fi x)**2 + (fi y)**2) / (2 * s**2)))

kernel :: ((Int,Int) -> Float) -> Int -> [Float]
kernel f r = [f (x,y) | y <- [-r..r] , x <- [-r..r] ]

-- given mask size, calculate kernel matrix
createMask :: (Float -> (Int,Int) -> Float) -> Float -> Int -> Matrix D32
createMask f s r =
  M.fromList (d,d) $ kernel (f s) r
  where
    d = 2 * r + 1

gaussianSmooth :: Float -> Int -> Image GrayScale D32 -> Image GrayScale D8
gaussianSmooth s r img = unsafeImageTo8Bit $ convolve2D (createMask g s r) (r,r) img

fromCVImage :: Image GrayScale D8 -> IO (PixelImage)
fromCVImage img = do
  saveImage "temp.png" img
  withGenImage img $ \pimg ->
    fromIplImage (castPtr pimg)

toCVImage :: PixelImage -> IO (Image RGB D8)
toCVImage img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

toCVImageG :: PixelImage -> IO (Image GrayScale D8)
toCVImageG img = creatingImage $ toBareImage $ toIplImage img
  where
    toBareImage :: IO (Ptr C'IplImage) -> IO (Ptr BareImage)
    toBareImage = liftM castPtr

getTrees :: QuadTree -> [QuadTree]
getTrees EmptyQuadTree = []
getTrees t
  | null cs = [t]
  | otherwise = cs
  where
    cs = concatMap getTrees $
      [ quadTreeChildNW t
      , quadTreeChildNE t
      , quadTreeChildSW t
      , quadTreeChildSE t ]

drawForestRegions :: [ForestSegment] -> Image RGB D8 -> Image RGB D32
drawForestRegions rs img =
  -- OpenCV uses BGR colors so must switch the order of RGB color components
  fimg <## [rectOp (c3,c2,c1) 1 (mkRectangle (x,y) (w,h))
    | (ForestSegment _ x y w h _ (c1,c2,c3)) <- rs]
  where
    fimg = unsafeImageTo32F img

drawRects :: Image GrayScale D32 -> [QuadTree] -> Image RGB D32
drawRects i ts =
  ri
  <## [rectOp (0,1,1) 1 r | r <- map toRect ts]
  where
    ri = grayToRGB i
    toRect (QuadTree _ x y s _ _ _ _ _ _) = mkRectangle (x,y) (s,s)

drawBlocks :: Image GrayScale D32 -> QuadForest -> Image RGB D32
drawBlocks img f =
  rimg
  -- <## [rectOp (0,1,1) (-1) r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  <## [rectOp (toColor m 255) (-1) r | (r,m) <- map toRect $ filter s ts]
  <## [circleOp (0,1,1) (x,y) r (Stroked 1) | (x,y,r) <- map (toCircle maxD) $ quadForestTrees f]
  <## concat [unsafePerformIO $ nlines t | t <- ts]
  where
    rimg = grayToRGB img
    ts = concatMap getTrees $ quadForestTrees f
    maxM = maximum $ map (mean . quadTreeStat) ts
    maxD = maximum $ map (deviation . quadTreeStat) $ quadForestTrees f

    toColor m maxM = (c,c,c) where c = double2Float $ m / maxM
    -- toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)
    s t = True -- (w == 1) && (h == 1)
    toRect (QuadTree _ x y s _ v _ _ _ _) = (mkRectangle (x,y) (s,s), mean v)
    toCircle maxD (QuadTree _ x y s _ v _ _ _ _) =
      (x+(s`div`2), y+(s`div`2), round $ (min maxD $ deviation v) / maxD * (fromIntegral s))
    nlines t@(QuadTree _ tx ty ts _ _ _ _ _ _) = do
      (ns::[QuadTree]) <- quadTreeNeighbors t
      return [lineOp (1,0,0) 1 (tx+(ts`div`2),ty+(ts`div`2)) (nx+(nd`div`2),ny+(nd`div`2))
          | n@(QuadTree _ nx ny nd _ _ _ _ _ _) <- ns]

drawForest :: String -> QuadForest -> IO ()
drawForest file forest = do
  fimg <- toCVImage =<< quadForestDrawImage True True forest
  --rf <- forestRegionsGet forest
  saveImage file fimg -- $ drawForestRegions rf fimg

treeEdge :: QuadForest -> Double -> QuadTree -> IO (Rectangle Int, Float)
treeEdge f m t = do
  --nstat <- quadTreeNeighborhoodStat f m t
  edge <- quadTreeEdgeResponse f t
  return (toRect t, double2Float $ edge) -- abs $ (deviation $ quadTreeStat t) - (deviation nstat))
  where
    toRect (QuadTree _ x y s _ _ _ _ _ _) = mkRectangle (x,y) (s,s)

drawEdges :: Image GrayScale D32 -> [(Rectangle Int, Float)] -> Image GrayScale D32
drawEdges img es =
  img
  <## [rectOp ((e - minE)/(maxE - minE)) (-1) r | (r,e) <- es]
  where
    minE = minimum $ map snd es
    maxE = maximum $ map snd es

main = do
  (sourceFile, targetFile, size, minSize, alpha, treeDiff, regionDiff) <- readArgs
  --(sourceFile, targetFile, sigma, size, minSize, threshold, alpha) <- readArgs
  img <- readFromFile sourceFile
  pimg <- fromCVImage $ unsafeImageTo8Bit img -- $ gaussianSmooth sigma 2 img
  forest <- quadForestCreate pimg size minSize
  withQuadForest forest $ \f -> do
    sf <- quadForestSegmentByOverlap alpha treeDiff regionDiff f
    --sf <- forestSegmentDeviation threshold minSize alpha f
    drawForest targetFile sf
    ss <- quadForestGetSegments sf
    let
      bySize (ForestSegment _ _ _ w h _ _) = w > 8 && w < 200 && h > 8 && h < 200
    rimg <- quadForestGetSegmentMask sf False $ filter bySize ss
    es <- mapM (treeEdge sf 2) $ concatMap getTrees $ quadForestTrees sf
    saveImage "segments.png" =<< toCVImageG rimg
    saveImage "edges.png" $ drawEdges img es
    saveImage "rects.png" $ drawRects img $ concatMap getTrees $ quadForestTrees sf
    saveImage "blocks.png" $ drawBlocks img sf
