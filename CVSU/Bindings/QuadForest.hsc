#include <bindings.dsl.h>
#include "cvsu_quad_forest.h"

module CVSU.Bindings.QuadForest where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.Integral
import CVSU.Bindings.List

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

#integral_t quad_forest_status

#starttype quad_forest
#field status          , <quad_forest_status>
#field original        , Ptr <pixel_image>
#field source          , Ptr <pixel_image>
#field integral        , <integral_image>
#field rows            , CULong
#field cols            , CULong
#field segments        , CULong
#field tree_max_size   , CULong
#field tree_min_size   , CULong
#field dx              , CULong
#field dy              , CULong
#field token           , CULong
#field trees           , <list>
#field links           , <list>
#field last_root_tree  , Ptr <list_item>
#field roots           , Ptr (Ptr <quad_tree>)
#stoptype

#ccall quad_forest_alloc , IO (Ptr <quad_forest>)

#ccall quad_forest_free , Ptr <quad_forest> -> IO ()

#ccall quad_forest_create , Ptr <quad_forest> -> Ptr <pixel_image> -> CULong \
  -> CULong -> IO <result>

#ccall quad_forest_reload , Ptr <quad_forest> ->  CULong -> CULong \
  -> IO <result>

#ccall quad_forest_destroy , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_nullify , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_is_null , Ptr <quad_forest> -> IO <truth_value>

#ccall quad_forest_set_init , Ptr <quad_forest> -> IO ()

#ccall quad_forest_set_update , Ptr <quad_forest> -> IO ()

#ccall quad_forest_set_parse , Ptr <quad_forest> -> IO ()

#ccall quad_forest_has_parse , Ptr <quad_forest> -> IO <truth_value>

#ccall quad_forest_update , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_get_segments , Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> IO <result>

#ccall quad_forest_get_segment_trees , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> IO <result>

#ccall quad_forest_get_segment_neighbors , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> IO <result>

#ccall quad_forest_get_segment_mask , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> <truth_value> -> IO <result>

#ccall quad_forest_get_segment_boundary , Ptr <quad_forest> \
  -> Ptr <quad_forest_segment> -> Ptr <list> -> IO <result>

#ccall quad_forest_get_links , Ptr <quad_forest> -> Ptr <list> \
  -> <link_visualization_mode> -> IO <result>

#ccall quad_forest_highlight_segments , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <segment>) -> CULong -> Ptr CUChar -> IO <result>

#ccall quad_forest_draw_image , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> <truth_value> -> IO <result>

#ccall quad_forest_draw_trees , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> IO <result>

#ccall quad_forest_find_edges , Ptr <quad_forest> -> CULong -> CDouble \
  -> <direction> -> IO <result>

#ccall quad_forest_find_boundaries , Ptr <quad_forest> -> CULong -> CDouble \
  -> CULong -> IO <result>

#ccall quad_forest_find_boundaries_with_hysteresis , Ptr <quad_forest> \
  -> CULong -> CDouble -> CDouble -> IO <result>

#ccall quad_forest_prune_boundaries , Ptr <quad_forest> -> IO <result>
