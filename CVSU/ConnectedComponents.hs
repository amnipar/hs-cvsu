module CVSU.ConnectedComponents
( ConnectedComponents(..)
, ConnectedComponent(..)
, allocConnectedComponents
, createConnectedComponents
, drawConnectedComponents
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.ConnectedComponents
import CVSU.PixelImage

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Concurrent

data ConnectedComponent =
  ConnectedComponent
  { componentX :: Int
  , componentY :: Int
  , componentW :: Int
  , componentH :: Int
  , componentColor :: (Float,Float,Float)
  }

data ConnectedComponents =
  ConnectedComponents
  { connectedPtr :: !(ForeignPtr C'connected_components)
  , connectedImage :: PixelImage
  , connectedWidth :: Int
  , connectedHeight :: Int
  , connectedComponents :: [ConnectedComponent]
  }

allocConnectedComponents :: IO (ForeignPtr C'connected_components)
allocConnectedComponents = do
  ptr <- c'connected_components_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'connected_components_free ptr)
    else error "Memory allocation failed in allocConnectedComponents"


createConnectedComponents :: PixelImage -> IO (ConnectedComponents)
createConnectedComponents img = do
  fcomp <- allocConnectedComponents
  withForeignPtr (imagePtr img) $ \pimg ->
    withForeignPtr fcomp $ \pcomp -> do
      r1 <- c'connected_components_create pcomp pimg
      if r1 /= c'SUCCESS
        then error "Creating connected components failed"
        else do
          r2 <- c'connected_components_update pcomp
          if r2 /= c'SUCCESS
            then error "Updating connected components failed"
            else do
              C'connected_components{
                c'connected_components'width = w,
                c'connected_components'height = h,
                c'connected_components'regions = ppreg,
                c'connected_components'count = c
              } <- peek pcomp
              preg <- peekArray (fromIntegral c) ppreg
              cs <- mapM ptrToConnectedComponent preg
              return $ ConnectedComponents fcomp img (fromIntegral w) (fromIntegral h) cs

ptrToConnectedComponent :: Ptr C'region_info -> IO (ConnectedComponent)
ptrToConnectedComponent preg = do
  (C'region_info _ _ x1 y1 x2 y2 _ _ _) <- peek preg
  return $ ConnectedComponent 
    (fromIntegral x1) 
    (fromIntegral y1) 
    (fromIntegral $ x2-x1) 
    (fromIntegral $ y2-y1) (1,1,1)

drawConnectedComponents :: ConnectedComponents -> IO (PixelImage)
drawConnectedComponents comp = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withForeignPtr (connectedPtr comp) $ \pcomp -> do
      r <- c'connected_components_draw_image pcomp pimg
      if r /= c'SUCCESS
         then error "Drawing connected components image failes"
         else ptrToPixelImage fimg
