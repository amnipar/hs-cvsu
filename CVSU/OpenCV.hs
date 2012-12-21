module CVSU.OpenCV 
( fromIplImage
) where

import CVSU.PixelImage
import CVSU.Bindings.Types
import CVSU.Bindings.OpenCV

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)

fromIplImage :: Ptr () -> IO (PixelImage)
fromIplImage ptr = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    result <- c'pixel_image_create_from_ipl_image pimg (castPtr ptr) c'GREY
    if result /= c'SUCCESS
       then error $ "fromIplImage failed with " ++ (show result)
       else ptrToPixelImage True fimg
