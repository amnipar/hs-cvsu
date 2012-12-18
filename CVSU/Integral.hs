module CVSU.Integral
( IntegralImage(..)
, createIntegralImage
, integralMeanByRect
, integralMeanByRadius
, integralVarianceByRect
, integralVarianceByRadius
, integralStatisticsByRect
, integralStatisticsByRadius
, integralThresholdSauvola
, integralThresholdFeng
) where

import CVSU.Bindings.Types
import CVSU.Bindings.Integral
import CVSU.PixelImage
import CVSU.Types

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal
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
integralMeanByRadius int r (cx,cy) = integralMeanByRect int (cx-r,cy-r) (s,s)
  where
    s = 2 * r + 1

integralVarianceByRect :: IntegralImage -> (Int,Int) -> (Int,Int) -> IO (Double)
integralVarianceByRect int (x,y) (w,h) =
  withForeignPtr (integralPtr int) $ \pint ->
    liftM realToFrac $ c'integral_image_calculate_variance pint
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0

integralVarianceByRadius :: IntegralImage -> Int -> (Int,Int) -> IO (Double)
integralVarianceByRadius int r (cx,cy) = integralVarianceByRect int (cx-r,cy-r) (s,s)
  where
    s = 2 * r + 1

integralStatisticsByRect :: IntegralImage -> (Int,Int) -> (Int,Int) -> IO (Statistics)
integralStatisticsByRect int (x,y) (w,h) =
  withForeignPtr (integralPtr int) $ \pint -> do
    let
      stat = (C'statistics 0 0 0 0 0 0)
    with stat $ \pstat -> do
      c'integral_image_calculate_statistics pint pstat
        (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0
      (C'statistics n s1 s2 m s d) <- peek pstat
      return $ Statistics (realToFrac n) (realToFrac s1) (realToFrac s2)
        (realToFrac m) (realToFrac s) (realToFrac d)

integralStatisticsByRadius :: IntegralImage -> Int -> (Int,Int) -> IO (Statistics)
integralStatisticsByRadius int r (cx,cy) = integralStatisticsByRect int (cx-r,cy-r) (s,s)
  where
    s = 2 * r + 1

integralThresholdSauvola :: Int -> Double -> IntegralImage -> IO (PixelImage)
integralThresholdSauvola r k int = do
  fimg <- allocPixelImage
  withForeignPtr (integralPtr int) $ \pint -> do
    withForeignPtr fimg $ \pimg -> do
      r <- c'integral_image_threshold_sauvola pint pimg (fromIntegral r) (realToFrac k)
      if r /= c'SUCCESS
        then error $ "Sauvola thresholding failed with " ++ (show r)
        else ptrToPixelImage True fimg

integralThresholdFeng :: Int -> Double -> IntegralImage -> IO (PixelImage)
integralThresholdFeng r k int = do
  fimg <- allocPixelImage
  withForeignPtr (integralPtr int) $ \pint -> do
    withForeignPtr fimg $ \pimg -> do
      r <- c'integral_image_threshold_feng pint pimg (fromIntegral r) (realToFrac k)
      if r /= c'SUCCESS
        then error $ "Feng thresholding failed with " ++ (show r)
        else ptrToPixelImage True fimg
