#include <bindings.dsl.h>
#include "cvsu_basic.h"

module CVSU.Bindings.PixelImage where

#strict_import

import CVSU.Bindings.Types

import Foreign.Ptr

#starttype pixel_image
#field parent   , Ptr <pixel_image>
#field data     , Ptr ()
#field rows     , Ptr Word8
#field own_data , CULong
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

#ccall pixel_image_create , Ptr <pixel_image> -> <pixel_type> -> \
  <pixel_format> -> CULong -> CULong -> CULong -> CULong -> IO (<result>)

#ccall pixel_image_create_from_data , Ptr <pixel_image> -> Ptr () -> \
  <pixel_type> -> <pixel_format> -> CULong -> CULong -> CULong -> CULong -> IO (<result>)

#ccall pixel_image_destroy , Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_nullify , Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_convert , Ptr <pixel_image> -> Ptr <pixel_image> -> IO (<result>)

#ccall pixel_image_clear , Ptr <pixel_image> -> IO (<result>)