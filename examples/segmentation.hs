{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.QuadForest
import CVSU.Moments

import CV.Image
import CV.CVSU
import CV.CVSU.Rectangle
import CV.Filters
import CV.Matrix as M
import CV.Drawing
import CV.ImageOp
import CV.Pixelwise
import Utils.Rectangle

import ReadArgs
import Data.List
import Data.Ord
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

toColor v maxV = (c,c,c) where c = realToFrac $ v / maxV --double2Float

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

drawSegments :: [ForestSegment] -> Image RGB D32 -> Image RGB D32
drawSegments rs img =
  -- OpenCV uses BGR colors so must switch the order of RGB color components
  img <## [rectOp (c3,c2,c1) 1 (mkRectangle (x,y) (w,h))
    | (ForestSegment _ x y w h _ (c1,c2,c3)) <- rs]

drawSegmentNeighbors :: QuadForest -> ForestSegment -> Image RGB D32 -> Image RGB D32
drawSegmentNeighbors f s img =
  img <## [lineOp (0,1,1) 1 sc nc | nc <- nsc]
  where
    rc (x,y) = (round x, round y)
    sc = rc $ momentCenter sm
    nsc = map (rc.momentCenter) nsm
    ns = unsafePerformIO $ quadForestGetSegmentNeighbors f [s]
    st = unsafePerformIO $ quadForestGetSegmentTrees f [s]
    nst = unsafePerformIO $ mapM ((quadForestGetSegmentTrees f).(:[])) ns
    sm = quadTreeMoments st
    nsm = map quadTreeMoments nst

drawTrees :: [QuadTree] -> Image RGB D32 -> Image RGB D32
drawTrees ts img =
  img <## [rectOp (toColor v maxV) (-1) r
    | (r,v) <- map (\t -> (shrink $ treeToRectangle t, mean $ quadTreeStat t)) ts]
  where
    shrink (Rectangle x y w h) = Rectangle (x+1) (y+1) (w-2) (h-2)
    maxV = maximum $ map (mean.quadTreeStat) ts

drawRects :: Image RGB D32 -> [QuadTree] -> Image RGB D32
drawRects img ts =
  img <## [rectOp (0,1,1) 1 r | r <- map treeToRectangle ts]

drawBlocks :: Image RGB D32 -> QuadForest -> Image RGB D32
drawBlocks img f =
  img
  -- <## [rectOp (0,1,1) (-1) r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  <## [rectOp (toColor m 255) (-1) r
    | (r,m) <- map (\t -> (treeToRectangle t, mean $ quadTreeStat t)) ts]
  <## [circleOp (0,1,1) (x,y) r (Stroked 1) | (x,y,r) <- map (toCircle maxD) $ quadForestTrees f]
  <## concat [unsafePerformIO $ nlines t | t <- ts]
  where
    ts = filter ((>8).quadTreeSize) $ concatMap getTrees $ quadForestTrees f
    maxM = maximum $ map (mean . quadTreeStat) ts
    maxD = maximum $ map (deviation . quadTreeStat) $ quadForestTrees f
    toCircle maxD (QuadTree _ x y s _ v _ _ _ _ _) =
      (x+(s`div`2), y+(s`div`2), round $ (min maxD $ deviation v) / maxD * (fromIntegral s))
    nlines t@(QuadTree _ tx ty ts _ _ _ _ _ _ _) = do
      (ns::[QuadTree]) <- quadTreeNeighbors t
      return [lineOp (1,0,0) 1 (tx+(ts`div`2),ty+(ts`div`2)) (nx+(nd`div`2),ny+(nd`div`2))
          | n@(QuadTree _ nx ny nd _ _ _ _ _ _ _) <- ns]

drawForest :: String -> QuadForest -> IO ()
drawForest file forest = do
  fimg <- expectByteRGB =<< fromPixelImage =<< quadForestDrawImage True True forest
  saveImage file fimg

treeEdge :: QuadForest -> Double -> QuadTree -> IO (Rectangle Int, Float)
treeEdge f m t = do
  --nstat <- quadTreeNeighborhoodStat f m t
  edge <- quadTreeEdgeResponse f t
  return (treeToRectangle t, double2Float $ edge) -- abs $ (deviation $ quadTreeStat t) - (deviation nstat))

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
  pimg <- toPixelImage $ unsafeImageTo8Bit img -- $ gaussianSmooth sigma 2 img
  forest <- quadForestCreate pimg size minSize
  withQuadForest forest $ \f -> do
    sf <- quadForestSegmentByOverlap alpha treeDiff regionDiff f
    --sf <- forestSegmentDeviation threshold minSize alpha f
    drawForest targetFile sf
    ss <- quadForestGetSegments sf
    let
      rimg = grayToRGB img
      bySize (ForestSegment _ _ _ w h _ _) = w > 8 && w < 200 && h > 8 && h < 200
      size (ForestSegment _ _ _ w h _ _) = w * h
      rs = reverse $ sortBy (comparing size) $ filter bySize ss
      s = head rs
    es <- mapM (treeEdge sf 2) $ concatMap getTrees $ quadForestTrees sf
    ts <- quadForestGetSegmentTrees sf rs
    rpimg <- toPixelImage $ unsafeImageTo8Bit rimg
    hpimg <- quadForestHighlightSegments sf rpimg (0,0,1) rs
    himg <- liftM unsafeImageTo32F $ expectByteRGB =<< fromPixelImage hpimg
    simg <- liftM unsafeImageTo32F $ expectByteRGB =<< fromPixelImage =<< 
        quadForestDrawImage True True sf
    saveImage "segments.png" $ drawSegmentNeighbors sf s simg
    saveImage "trees.png" $ drawTrees ts $ drawSegments rs himg
    saveImage "edges.png" $ drawEdges img es
    saveImage "rects.png" $ drawRects rimg $ concatMap getTrees $ quadForestTrees sf
    saveImage "blocks.png" $ drawBlocks rimg sf
