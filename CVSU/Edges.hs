module CVSU.Edges
( Orientation(..)
, Edge(..)
, EdgeImage(..)
, allocEdgeImage
, createEdgeImage
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.Edges
import CVSU.PixelImage

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Maybe
import System.IO.Unsafe

data Orientation = HEdge | VEdge deriving Eq

data Edge =
  Edge
  { x :: Int
  , y :: Int
  , orientation :: Orientation
  , value :: Float
  } deriving Eq

data EdgeImage =
  NullEdge |
  EdgeImage
  { edgePtr :: !(ForeignPtr C'edge_image)
  , original :: PixelImage
  , hstep :: Int
  , vstep :: Int
  , hmargin :: Int
  , vmargin :: Int
  , boxWidth :: Int
  , boxHeight :: Int
  , dx :: Int
  , dy :: Int
  , hedges :: [[Edge]]
  , vedges :: [[Edge]]
  }

allocEdgeImage :: IO (Maybe (ForeignPtr C'edge_image))
allocEdgeImage = do
  ptr <- c'edge_image_alloc
  if ptr /= nullPtr
    then do
      foreignPtr <- newForeignPtr p'edge_image_free ptr
      return $ Just foreignPtr
    else do
      return Nothing

createEdgeImage :: Int -> Int -> Int -> Int -> Int -> Int -> PixelImage -> IO (EdgeImage)
createEdgeImage hstep vstep hmargin vmargin bwidth blength i = do
  e <- allocEdgeImage
  if isNothing e
    then do
      return NullEdge
    else do
      withForeignPtr (fromJust e) $ \e_ptr ->
        withForeignPtr (imagePtr i) $ \i_ptr -> do
          r <- c'edge_image_create e_ptr i_ptr
            (fromIntegral hstep)
            (fromIntegral vstep)
            (fromIntegral hmargin)
            (fromIntegral vmargin)
            (fromIntegral bwidth)
            (fromIntegral blength)
          if r /= c'SUCCESS
            then return NullEdge
            else do
              r <- c'edge_image_update e_ptr
              if r /= c'SUCCESS
                then return NullEdge
                else ptrToEdgeImage i (fromJust e)

eValue :: Ptr CSChar -> Int -> Float
eValue p o = fromIntegral $ unsafePerformIO $ peek (advancePtr p o)

imageToVEdgeList :: Int -> Int -> Int -> C'pixel_image -> [[Edge]]
imageToVEdgeList step hmargin vmargin
  C'pixel_image
  { c'pixel_image'data = d
  , c'pixel_image'width = w
  , c'pixel_image'height = h
  , c'pixel_image'stride = s
  } = [[toVEdge d (x,vmargin+y*step,y*(fromIntegral s)+x) | x <- [hmargin..(fromIntegral w)-hmargin-1]] | y <- [0..(fromIntegral h)-1]]
  where
    toVEdge ptr (x,y,offset) = (Edge x y VEdge $ eValue (castPtr ptr) offset)

imageToHEdgeList :: Int -> Int -> Int -> C'pixel_image -> [[Edge]]
imageToHEdgeList step hmargin vmargin
  C'pixel_image
  { c'pixel_image'data = d
  , c'pixel_image'width = w
  , c'pixel_image'height = h
  , c'pixel_image'stride = s
  } = [[toHEdge d (hmargin+x*step,y,y*(fromIntegral s)+x) | y <- [vmargin..(fromIntegral h)-vmargin-1]] | x <- [0..(fromIntegral w)-1]]
  where
    toHEdge ptr (x,y,offset) = (Edge x y HEdge $ eValue (castPtr ptr) offset)

ptrToEdgeImage :: PixelImage -> ForeignPtr C'edge_image -> IO (EdgeImage)
ptrToEdgeImage i fptr = do
  withForeignPtr fptr $ \ptr ->
    if ptr == nullPtr then return NullEdge
    else do
      C'edge_image{
      c'edge_image'hedges = ih,
      c'edge_image'vedges = iv,
      c'edge_image'width = w,
      c'edge_image'height = h,
      c'edge_image'hstep = hs,
      c'edge_image'vstep = vs,
      c'edge_image'hmargin = hm,
      c'edge_image'vmargin = vm,
      c'edge_image'box_width = bw,
      c'edge_image'box_length = bl,
      c'edge_image'dx = dx,
      c'edge_image'dy = dy
      } <- peek ptr
      return $ (EdgeImage fptr i
        (fromIntegral hs)
        (fromIntegral vs)
        (fromIntegral hm)
        (fromIntegral vm)
        (fromIntegral bw)
        (fromIntegral bl)
        (fromIntegral dx)
        (fromIntegral dy)
        (imageToHEdgeList (fromIntegral hs) (fromIntegral (dx+hm)) (fromIntegral (dy+vm)) ih)
        (imageToVEdgeList (fromIntegral vs) (fromIntegral (dx+hm)) (fromIntegral (dy+vm)) iv))

