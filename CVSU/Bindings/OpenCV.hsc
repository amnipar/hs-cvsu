#include <bindings.dsl.h>
#include "cvsu_opencv.h"

module CVSU.Bindings.OpenCV where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

import Foreign.Ptr

#opaque_t IplImage

#ccall pixel_image_create_from_ipl_image , \
  Ptr <pixel_image> -> Ptr <IplImage> -> <pixel_format> -> IO <result>
