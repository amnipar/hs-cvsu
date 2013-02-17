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

#starttype quad_forest_segment
#field parent       , Ptr <quad_forest_segment>
#field rank         , CULong
#field x1           , CULong
#field y1           , CULong
#field x2           , CULong
#field y2           , CULong
#field stat         , <statistics>
#field devmean      , CDouble
#field devdev       , CDouble
#field has_boundary , <truth_value>
#field color[0]     , Word8
#field color[1]     , Word8
#field color[2]     , Word8
#stoptype

#starttype quad_forest_edge
#field parent    , Ptr <quad_forest_edge>
#field rank      , CULong
#field dx        , CDouble
#field dy        , CDouble
#field mag       , CDouble
#field ang       , CDouble
#field mean      , CDouble
#field deviation , CDouble
#field has_edge  , <truth_value>
#field dir       , <direction>
#stoptype

#starttype quad_tree
#field x           , CULong
#field y           , CULong
#field size        , CULong
#field level       , CULong
#field stat        , <statistics>
#field segment     , <quad_forest_segment>
#field edge        , <quad_forest_edge>
#field parent      , Ptr <quad_tree>
#field nw          , Ptr <quad_tree>
#field ne          , Ptr <quad_tree>
#field sw          , Ptr <quad_tree>
#field se          , Ptr <quad_tree>
#field n           , Ptr <quad_tree>
#field e           , Ptr <quad_tree>
#field s           , Ptr <quad_tree>
#field w           , Ptr <quad_tree>
#field pool        , CDouble
#field pool2       , CDouble
#field acc         , CDouble
#field acc2        , CDouble
#stoptype

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
#field trees           , <list>
#field last_root_tree  , Ptr <list_item>
#field roots           , Ptr (Ptr <quad_tree>)
#stoptype

#ccall quad_tree_nullify , Ptr <quad_tree> -> IO <result>

#ccall quad_tree_is_null , Ptr <quad_tree> -> IO <truth_value>

#ccall quad_tree_divide , Ptr <quad_forest> -> Ptr <quad_tree> -> IO <result>

#ccall quad_tree_has_children , Ptr <quad_tree> -> IO <truth_value>

#ccall quad_tree_get_child_statistics , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> Ptr <quad_tree> -> IO <result>

#ccall quad_tree_get_neighborhood_statistics , Ptr <quad_forest> \
  -> Ptr <quad_tree> -> Ptr <statistics> -> CDouble -> IO <result>

#ccall quad_tree_divide_with_overlap , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> CDouble -> CDouble -> IO <result>

#ccall quad_tree_get_edge_response , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> Ptr CDouble -> Ptr CDouble -> IO <result>

#ccall quad_tree_get_child_edge_response , Ptr <quad_forest> \
  -> Ptr <quad_tree> -> Ptr CDouble -> Ptr CDouble -> IO <result>

#ccall quad_tree_get_neighbors , Ptr <list> -> Ptr <quad_tree> -> IO <result>

#ccall quad_tree_segment_create , Ptr <quad_tree> -> IO ()

#ccall quad_forest_segment_union , Ptr <quad_forest_segment> \
  -> Ptr <quad_forest_segment> -> IO ()

#ccall quad_tree_segment_union , Ptr <quad_tree> -> Ptr <quad_tree> -> IO ()

#ccall quad_tree_segment_find , Ptr <quad_tree> \
  -> IO (Ptr <quad_forest_segment>)

#ccall quad_tree_segment_get , Ptr <quad_tree> -> IO CULong

#ccall quad_tree_is_segment_parent , Ptr <quad_tree> -> IO <truth_value>

#ccall quad_forest_alloc , IO (Ptr <quad_forest>)

#ccall quad_forest_free , Ptr <quad_forest> -> IO ()

#ccall quad_forest_create , Ptr <quad_forest> -> Ptr <pixel_image> -> CULong \
  -> CULong -> IO <result>

#ccall quad_forest_reload , Ptr <quad_forest> ->  CULong -> CULong \
  -> IO <result>

#ccall quad_forest_refresh_segments , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_destroy , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_nullify , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_is_null , Ptr <quad_forest> -> IO <truth_value>

#ccall quad_forest_update , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_segment_with_deviation , Ptr <quad_forest> -> CDouble \
  -> CDouble -> IO <result>

#ccall quad_forest_segment_with_overlap , Ptr <quad_forest> -> CDouble \
  -> CDouble -> CDouble -> IO <result>

#ccall quad_forest_get_segments , Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> IO <result>

#ccall quad_forest_get_segment_trees , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> IO <result>

#ccall quad_forest_get_segment_neighbors , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> IO <result>

#ccall quad_forest_draw_trees , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> IO <result>

#ccall quad_forest_get_segment_mask , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> <truth_value> -> IO <result>

#ccall quad_forest_get_segment_boundary , Ptr <quad_forest> \
  -> Ptr <quad_forest_segment> -> Ptr <list> -> IO <result>

#ccall quad_forest_highlight_segments , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <quad_forest_segment>) -> CULong -> Ptr CUChar -> IO <result>

#ccall quad_forest_draw_image , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> <truth_value> -> IO <result>

#ccall quad_forest_find_edges , Ptr <quad_forest> -> CULong -> CDouble \
  -> <direction> -> IO <result>

#ccall quad_forest_find_boundaries , Ptr <quad_forest> -> CULong -> CDouble \
  -> IO <result>

#ccall quad_forest_segment_edges , Ptr <quad_forest> -> CULong \
  -> CDouble -> <direction> -> CULong -> CDouble -> <direction> -> <direction> \
  -> IO <result>

#ccall quad_forest_segment_with_boundaries , Ptr <quad_forest> -> CULong \
  -> CDouble -> CDouble -> CDouble -> CDouble -> IO <result>
