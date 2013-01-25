#include <bindings.dsl.h>
  #include "cvsu_pixel_image.h"

module CVSU.Bindings.PixelImage where

#strict_import

import CVSU.Bindings.Types

import Foreign.C.String
import Foreign.Ptr

#starttype pixel_image
#field parent   , Ptr <pixel_image>
#field data     , Ptr ()
#field rows     , Ptr Word8
#field type     , <pixel_type>
#field format   , <pixel_format>
#field dx       , CULong
#field dy       , CULong
#field width    , CULong
#field height   , CULong
#field offset   , CULong
#field step     , CULong
#field stride   , CULong
#field size     , CULong
#stoptype

#ccall pixel_image_alloc , IO (Ptr <pixel_image>)

#ccall pixel_image_free , Ptr <pixel_image> -> IO ()

#ccall pixel_image_create , Ptr <pixel_image> -> <pixel_type> -> <pixel_format> \
  -> CULong -> CULong -> CULong -> CULong -> IO (<result>)

#ccall pixel_image_create_from_data , Ptr <pixel_image> -> Ptr () -> <pixel_type> \
  -> <pixel_format> -> CULong -> CULong -> CULong -> CULong -> IO (<result>)

#ccall pixel_image_destroy , Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_nullify , Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_is_null , Ptr <pixel_image> -> IO <truth_value>

#ccall pixel_image_create_roi , Ptr <pixel_image> -> Ptr <pixel_image> \
  -> CULong -> CULong -> CULong -> CULong -> IO <result>

#ccall pixel_image_convert , Ptr <pixel_image> -> Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_clear , Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_read , Ptr <pixel_image> -> CString -> IO <result>

#ccall pixel_image_write , Ptr <pixel_image> -> CString -> <truth_value> \
  -> IO <result>

#ccall pixel_image_find_min_byte , Ptr <pixel_image> -> CLong -> CLong \
  -> CLong -> CLong -> CULong -> IO (CDouble)

#ccall pixel_image_find_max_byte , Ptr <pixel_image> -> CLong -> CLong \
  -> CLong -> CLong -> CULong -> IO (CDouble)

#ccall pixel_image_calculate_mean_byte , Ptr <pixel_image> -> CLong -> CLong \
  -> CLong -> CLong -> CULong -> IO (CDouble)

#ccall pixel_image_calculate_variance_byte , Ptr <pixel_image> -> CLong -> CLong \
  -> CLong -> CLong -> CULong -> IO (CDouble)
