#include <bindings.dsl.h>
#include "cvsu_connected_components.h"

module CVSU.Bindings.ConnectedComponents where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import Foreign.Ptr
import Foreign.C.Types

#starttype region_info
#field id , Ptr <region_info>
#field rank , CULong
#field x1 , CULong
#field y1 , CULong
#field x2 , CULong
#field y2 , CULong
#field value , Ptr Word8
#field is_border , CULong
#field color , Ptr Word8
#stoptype

#starttype connected_components
#field original , Ptr <pixel_image>
#field width    , CULong
#field height   , CULong
#field channels , CULong
#field pixels   , Ptr <region_info>
#field regions  , Ptr (Ptr <region_info>)
#field count    , CULong
#stoptype

#ccall connected_components_alloc , IO (Ptr <connected_components>)

#ccall connected_components_free , Ptr <connected_components> -> IO ()

#ccall connected_components_create , Ptr <connected_components> \
  -> Ptr <pixel_image> -> IO <result>

#ccall connected_components_destroy , Ptr <connected_components> -> IO <result>

#ccall connected_components_update , Ptr <connected_components> -> IO <result>

#ccall connected_components_draw_image , Ptr <connected_components> \
  -> Ptr <pixel_image> -> IO <result>
