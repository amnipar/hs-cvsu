#include <bindings.dsl.h>
#include "cvsu_integral.h"

module CVSU.Bindings.Integral where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

import Foreign.Ptr

#starttype integral_image
#field original , Ptr <pixel_image>
#field I_1      , <pixel_image>
#field I_2      , <pixel_image>
#field width    , CULong
#field height   , CULong
#field step     , CULong
#field stride   , CULong
#stoptype

#ccall integral_image_alloc , IO (Ptr <integral_image>)

#ccall integral_image_free , Ptr <integral_image> -> IO ()

#ccall integral_image_create , Ptr <integral_image> -> Ptr <pixel_image> \
  -> IO <result>

#ccall integral_image_destroy , Ptr <integral_image> -> IO <result>

#ccall integral_image_nullify , Ptr <integral_image> -> IO <result>

#ccall integral_image_is_null , Ptr <integral_image> -> IO <truth_value>

#ccall integral_image_clone , Ptr <integral_image> -> Ptr <integral_image> \
  -> IO <result>

#ccall integral_image_copy , Ptr <integral_image> -> Ptr <integral_image> \
  -> IO <result>

#ccall integral_image_update , Ptr <integral_image> -> IO <result>

#ccall integral_image_calculate_mean , Ptr <integral_image> -> CLong -> CLong \
  -> CLong -> CLong -> CULong -> IO CDouble

#ccall integral_image_calculate_variance , Ptr <integral_image> -> CLong \
  -> CLong -> CLong -> CLong -> CULong -> IO CDouble

#ccall integral_image_calculate_statistics , Ptr <integral_image> \
  -> Ptr <statistics> -> CLong -> CLong -> CLong -> CLong -> CULong -> IO ()

#ccall integral_image_threshold_sauvola , Ptr <integral_image> \
  -> Ptr <pixel_image> -> <truth_value> -> CULong -> CDouble -> <truth_value> \
  -> CDouble -> <truth_value> -> IO <result>

#ccall integral_image_threshold_feng , Ptr <integral_image> \
  -> Ptr <pixel_image> -> <truth_value> -> CULong -> CDouble -> <truth_value> \
  -> CDouble -> IO <result>
