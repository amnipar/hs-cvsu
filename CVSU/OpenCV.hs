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

fromIplImage :: Ptr () -> IO (PixelImage)
fromIplImage ptr = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    result <- c'pixel_image_create_from_ipl_image pimg (castPtr ptr) c'GREY
    if result /= c'SUCCESS
       then error $ "fromIplImage failed with " ++ (show result)
       else ptrToPixelImage fimg

toIplImage :: PixelImage -> IO (Ptr (C'IplImage))
toIplImage img = do
  withForeignPtr (imagePtr img) $ \pimg -> do
    let
      mallocIplImage :: IO (Ptr (Ptr C'IplImage))
      mallocIplImage = malloc
    ppipl <- mallocIplImage
    r <- c'ipl_image_create_from_pixel_image ppipl pimg c'RGB
    if r /= c'SUCCESS
       then error "Creating IplImage failed"
       else do
         pipl <- peek ppipl
         return pipl
