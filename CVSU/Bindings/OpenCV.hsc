#include <bindings.dsl.h>
#include "cvsu_opencv.h"

module CVSU.Bindings.OpenCV
( c'pixel_image_create_from_ipl_image
, c'pixel_image_create_from_file
, c'pixel_image_write_to_file
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#opaque_t IplImage

#ccall pixel_image_create_from_ipl_image , \
  Ptr <pixel_image> -> Ptr <IplImage> -> <pixel_format> -> IO <result>

#ccall pixel_image_create_from_file , \
  Ptr <pixel_image> -> CString -> <pixel_format> -> <pixel_type> -> IO <result>

#ccall pixel_image_write_to_file , \
  Ptr <pixel_image> -> CString -> IO <result>
