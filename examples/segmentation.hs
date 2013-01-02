{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.ImageTree
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

colorList = concat $ repeat
  [ (0,0,1), (0,1,0), (1,0,0), (0,1,1), (1,0,1), (1,1,0)
  , (0.5,0.5,1), (0.5,1,0.5), (1,0.5,0.5), (1,0.5,1), (1,1,0.5)
  , (0,0,0.75), (0,0.75,0), (0.75,0,0), (0,0.75,0.75), (0.75,0,0.75), (0.75,0.75,0)
  , (0.25,0.25,0.75),(0.25,0.75,0.25),(0.75,0.25,0.25),(0.25,0.75,0.75),(0.75,0.25,0.75),(0.75,0.75,0.25)
  , (0,0,0.5), (0,0.5,0), (0.5,0,0), (0,0.5,0.5), (0.5,0,0.5), (0.5,0.5,0)
  , (0,0,0.25), (0,0.25,0), (0.25,0,0), (0,0.25,0.25), (0.25,0,0.25), (0.25,0.25,0)
  ]

drawRegions :: Image GrayScale D32 -> [ImageTree Statistics] -> Image RGB D32
drawRegions i ts =
  ri <## [rectOp (treeColor colors c) (-1)
      (mkRectangle (x,y) (w-1,h-1)) |
      ImageTree{classId=c,block=(ImageBlock x y w h _)} <- ts]
  where
    ri = grayToRGB i
    colors = regionColors ts colorList
    assignColor (ts,(c:cs)) t =
      case (lookup (classId t) ts) of
        Just rc -> (ts,(c:cs))
        Nothing -> (((classId t),c):ts,cs)
    treeColor cs cid
      | cid == 0 = (0,0,0)
      | otherwise = 
        case (lookup cid cs) of
          Just c  -> c
          Nothing -> (0,0,0)
    regionColors ts cs = fst $ foldl assignColor ([],cs) $ filter ((/=0).classId) ts

drawForestRegions :: [ForestRegion] -> Image RGB D8 -> Image RGB D32
drawForestRegions rs img =
  -- OpenCV uses BGR colors so must switch the order of RGB color components
  fimg <## [rectOp (c3,c2,c1) 1 (mkRectangle (x,y) (w,h)) 
    | (ForestRegion _ x y w h _ (c1,c2,c3)) <- rs]
  where
    fimg = unsafeImageTo32F img

getTrees :: ImageTree a -> [ImageTree a]
getTrees EmptyTree = []
getTrees t
  | null cs = [t]
  | otherwise = cs
  where
    cs = concatMap getTrees [nw t, ne t, sw t, se t]

drawRects :: Image GrayScale D32 -> [ImageTree a] -> Image RGB D32
drawRects i ts =
  ri
  <## [rectOp (0,1,1) 1 r | r <- map (toRect.block) ts]
  where
    ri = grayToRGB i
    toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)

drawBlocks :: Image GrayScale D32 -> ImageForest Statistics -> Image RGB D32
drawBlocks img f =
  rimg
  -- <## [rectOp (0,1,1) (-1) r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  <## [rectOp (toColor m 255) (-1) r | (r,m) <- map toRect $ filter s $ map block ts]
  <## [circleOp (0,1,1) (x,y) r (Stroked 1) | (x,y,r) <- map ((toCircle maxD).block) $ trees f]
  <## concat [unsafePerformIO $ nlines t | t <- ts]
  where
    rimg = grayToRGB img
    ts = concatMap getTrees $ trees f
    maxM = maximum $ map (mean . value . block) ts
    maxD = maximum $ map (deviation . value . block) $ trees f

    toColor m maxM = (c,c,c) where c = double2Float $ m / maxM
    -- toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)
    s (ImageBlock _ _ w h _) = True -- (w == 1) && (h == 1)
    toRect (ImageBlock x y w h v) = (mkRectangle (x,y) (w,h), mean v)
    toCircle maxD (ImageBlock x y w h v) =
      (x+(w`div`2), y+(h`div`2), round $ (min maxD $ deviation v) / maxD * (fromIntegral w))
    nlines t@ImageTree{block=ImageBlock{x=tx,y=ty,w=tw,h=th}} = do
      (ns::[ImageTree Statistics]) <- treeNeighbors t
      return [lineOp (1,0,0) 1 (tx+(tw`div`2),ty+(th`div`2)) (nx+(nw`div`2),ny+(nh`div`2))
        | n@ImageBlock{x=nx,y=ny,w=nw,h=nh} <- map block ns]

drawForest :: String -> ImageForest Statistics -> IO ()
drawForest file forest = do
  fimg <- toCVImage =<< forestDrawImage True True forest
  saveImage file fimg -- $ drawForestRegions rf fimg

main = do
  (sourceFile, targetFile, size, sigma) <- readArgs
  -- print $ createMask g sigma 2
  img <- readFromFile sourceFile
  pimg <- fromCVImage $ gaussianSmooth sigma 2 img
  forest <- createForest pimg (size,size)
  withForest forest $ \f -> do
    sf <- forestSegmentEntropy 4 f
    drawForest targetFile sf
    --rf <- forestRegionsGet sf
    --fimg <- toCVImage =<< forestDrawImage False False sf
    --saveImage targetFile fimg -- $ drawForestRegions rf fimg
    -- drawRegions img $ concatMap getTrees $ trees sf
    --saveImage "rects.png" $ drawRects img $ concatMap getTrees $ trees sf
    --saveImage "blocks.png" $ drawBlocks img sf
