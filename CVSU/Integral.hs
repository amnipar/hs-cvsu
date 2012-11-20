module CVSU.Integral
( IntegralImage(..)
, createIntegralImage
, integralMeanByRect
, integralMeanByRadius
) where

import CVSU.Bindings.Types
import CVSU.Bindings.Integral
import CVSU.PixelImage

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Concurrent
import Control.Monad

data IntegralImage =
  IntegralImage
  { integralPtr :: !(ForeignPtr C'integral_image)
  , integralOriginal :: PixelImage
  , integralWidth :: Int
  , integralHeight :: Int
  }

allocIntegralImage :: IO (ForeignPtr C'integral_image)
allocIntegralImage = do
  ptr <- c'integral_image_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'integral_image_free ptr)
    else error "Memory allocation failed in allocIntegralImage"

createIntegralImage :: PixelImage -> IO (IntegralImage)
createIntegralImage img = do
  fint <- allocIntegralImage
  withForeignPtr (imagePtr img) $ \pimg ->
    withForeignPtr fint $ \pint -> do
      r1 <- c'integral_image_create pint pimg
      if r1 /= c'SUCCESS
        then error "Creating integral image failed"
        else do
          r2 <- c'integral_image_update pint
          if r2 /= c'SUCCESS
            then error "Updating integral image failed"
            else do
              C'integral_image{
                c'integral_image'width = w,
                c'integral_image'height = h
              } <- peek pint
              return $ IntegralImage fint img (fromIntegral w) (fromIntegral h)

integralMeanByRect :: IntegralImage -> (Int,Int) -> (Int,Int) -> IO (Double)
integralMeanByRect int (x,y) (w,h) =
  withForeignPtr (integralPtr int) $ \pint ->
    liftM realToFrac $ c'integral_image_calculate_mean pint
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0

integralMeanByRadius :: IntegralImage -> Int -> (Int,Int) -> IO (Double)
integralMeanByRadius int r (cx,cy) = integralMeanByRect int (cx-r,cy-r) (cx+r,cy+r)

--integralStatisticsByRect :: IntegralImage -> (Int,Int) -> (Int,Int) -> Statistics
