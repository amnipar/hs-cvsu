#include <bindings.dsl.h>
#include "cvsu_edges.h"

module CVSU.Bindings.Edges where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.Integral
import Foreign.Ptr

#starttype edge_image
#field I , <integral_image>
#field hedges , <pixel_image>
#field vedges , <pixel_image>
#field width , CULong
#field height , CULong
#field hstep , CULong
#field vstep , CULong
#field hmargin , CULong
#field vmargin , CULong
#field box_width , CULong
#field box_length , CULong
#field dx , CULong
#field dy , CULong
#stoptype

#ccall edge_image_alloc , IO (Ptr <edge_image>)

#ccall edge_image_free , Ptr <edge_image> -> IO ()

#ccall edge_image_create , Ptr <edge_image> -> Ptr <pixel_image> -> \
  CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO <result>

#ccall edge_image_destroy , Ptr <edge_image> -> IO <result>

#ccall edge_image_nullify , Ptr <edge_image> -> IO <result>

#ccall edge_image_is_null , Ptr <edge_image> -> IO <truth_value>

#ccall edge_image_clone , Ptr <edge_image> -> Ptr <edge_image> -> IO <result>

#ccall edge_image_copy , Ptr <edge_image> -> Ptr <edge_image> -> IO <result>

#ccall edge_image_update , Ptr <edge_image> -> IO <result>

#ccall edge_image_convert_to_grey8 , Ptr <edge_image> -> Ptr <pixel_image> \
  -> Ptr <pixel_image> -> IO <result>

#ccall edge_image_overlay_to_grey8 , Ptr <edge_image> -> Ptr <pixel_image> \
  -> IO <result>
