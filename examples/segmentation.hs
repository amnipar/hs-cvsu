{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage
import CVSU.ImageTree

import CV.Image
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle

import ReadArgs
import System.IO.Unsafe
import GHC.Float

getTrees :: ImageTree a -> [ImageTree a]
getTrees EmptyTree = []
getTrees t
  | null cs = [t]
  | otherwise = cs
  where
    cs = concatMap getTrees [nw t, ne t, sw t, se t]


drawBlocks :: Image RGB D32 -> ImageForest Statistics -> Image RGB D32
drawBlocks i f =
  i
  -- <## [rectOp (0,1,1) (-1) r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  <## [rectOp (toColor m 255) (-1) r | (r,m) <- map (toRect.block) ts]
  <## [circleOp (0,1,1) (x,y) r (Stroked 1) | (x,y,r) <- map ((toCircle maxD).block) ts]
  <## concat [unsafePerformIO $ nlines t | t <- ts]
  where
    ts = concatMap getTrees $ trees f
    maxM = maximum $ map (mean . value . block) ts
    maxD = maximum $ map (deviation . value . block) ts

    toColor m maxM = (c,c,c) where c = double2Float $ m / maxM
    -- toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)
    toRect (ImageBlock x y w h v) = (mkRectangle (x,y) (w,h), mean v)
    toCircle maxD (ImageBlock x y w h v) =
      (x+(w`div`2), y+(h`div`2), round $ (min maxD $ deviation v) / maxD * (fromIntegral w))
    nlines t@ImageTree{block=ImageBlock{x=tx,y=ty,w=tw,h=th}} = do
      (ns::[ImageTree Statistics]) <- treeNeighbors t
      return [lineOp (1,0,0) 1 (tx+(tw`div`2),ty+(th`div`2)) (nx+(nw`div`2),ny+(nh`div`2))
        | n@ImageBlock{x=nx,y=ny,w=nw,h=nh} <- map block ns]

main = do
  (sourceFile, targetFile, size) <- readArgs
  img <- readFromFile sourceFile
  pimg <- readPixelImage sourceFile
  forest <- createForest pimg (size,size)
  withForest forest $ \f -> do
    saveImage targetFile $ drawBlocks img f