#include <bindings.dsl.h>
#include "cvsu_quad_tree.h"

module CVSU.Bindings.QuadTree where

#strict_import

import CVSU.Bindings.Types

import Foreign.Ptr


#starttype quad_tree
#field x           , CULong
#field y           , CULong
#field size        , CULong
#field level       , CULong
#field stat        , <statistics>
#field segment     , <quad_forest_segment>
#field edge        , <quad_forest_edge>
#field intersection , <quad_forest_intersection>
#field annotation  , <tree_annotation>
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
#field links       , <list>
#field context     , <parse_context>
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
