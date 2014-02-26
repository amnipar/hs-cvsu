#include <bindings.dsl.h>
#include "cvsu_temporal_forest.h"

module CVSU.Bindings.TemporalForest where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.QuadForest
import CVSU.Bindings.List

import Foreign.Ptr
import Foreign.ForeignPtr

#opaque_t background_forest

#starttype temporal_forest
#field background    , Ptr <background_forest>
#field forests       , Ptr <quad_forest>
#field visual        , <pixel_image>
#field rows          , CULong
#field cols          , CULong
#field tree_max_size , CULong
#field tree_min_size , CULong
#field dx            , CULong
#field dy            , CULong
#field count         , CULong
#field current       , CULong
#field frames        , CULong
#stoptype

#ccall temporal_forest_alloc , IO (Ptr <temporal_forest>)

#ccall temporal_forest_free , Ptr <temporal_forest> -> IO ()

#ccall temporal_forest_create , Ptr <temporal_forest> -> Ptr <pixel_image> \
  -> CULong -> CULong -> CULong -> CULong -> IO <result>

#ccall temporal_forest_destroy , Ptr <temporal_forest> -> IO <result>

#ccall temporal_forest_nullify , Ptr <temporal_forest> -> IO <result>

#ccall temporal_forest_is_null , Ptr <temporal_forest> -> IO <truth_value>

#ccall temporal_forest_update , Ptr <temporal_forest> -> Ptr <pixel_image> \
  -> IO <result>

#ccall temporal_forest_visualize , Ptr <temporal_forest> -> IO <result>

#ccall temporal_forest_get_current , Ptr <temporal_forest> \
  -> IO (Ptr <quad_forest>)

#ccall temporal_forest_segment_count , Ptr <temporal_forest> -> IO CULong

#ccall temporal_forest_get_segments , Ptr <temporal_forest> \
  -> Ptr (Ptr <segment>) -> IO <result>

#ccall temporal_forest_get_segment_boundary , Ptr <temporal_forest> \
  -> Ptr <segment> -> Ptr <list> -> IO <result>
