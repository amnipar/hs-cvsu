module CVSU.OpenCV
( C'IplImage(..)
, fromIplImage
, toIplImage
) where

import CVSU.PixelImage
import CVSU.Bindings.Types
import CVSU.Bindings.OpenCV

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc



fromIplImage :: PixelFormat -> Ptr () -> IO (PixelImage)
fromIplImage format ptr = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    result <- c'pixel_image_create_from_ipl_image pimg (castPtr ptr) (cPixelFormat format)
    if result /= c'SUCCESS
       then error $ "fromIplImage failed with " ++ (show result)
       else ptrToPixelImage fimg

toIplImage :: PixelFormat -> PixelImage -> IO (Ptr (C'IplImage))
toIplImage format img = do
  withForeignPtr (imagePtr img) $ \pimg -> do
    let
      mallocIplImage :: IO (Ptr (Ptr C'IplImage))
      mallocIplImage = malloc
    ppipl <- mallocIplImage
    r <- c'ipl_image_create_from_pixel_image ppipl pimg (cPixelFormat format)
    if r /= c'SUCCESS
       then error "Creating IplImage failed"
       else do
         pipl <- peek ppipl
         return pipl
