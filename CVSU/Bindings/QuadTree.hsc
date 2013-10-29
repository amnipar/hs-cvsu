#include <bindings.dsl.h>
#include "cvsu_quad_tree.h"

module CVSU.Bindings.QuadTree where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.Annotation
import Foreign.Ptr

#starttype quad_tree_link_head
#field link       , <quad_tree_link>
#field opposite   , <quad_tree_link_head>
#field other      , <quad_tree_link_head>
#field tree       , <quad_tree>
#field angle      , CDouble
#field annotation , <typed_pointer>
#stoptype

#starttype quad_tree_link
#field a          , <quad_tree_link_head>
#field b          , <quad_tree_link_head>
#field category   , <direction>
#field distance   , CDouble
#field annotation , <typed_pointer>
#stoptype

#ccall quad_tree_link_destroy , <quad_tree_link> -> IO ()

#integral_t link_visualization_mode
#num v_LINK_NONE
#num v_LINK_DISTANCE
#num v_LINK_ANGLE_COST
#num v_LINK_SIMILARITY
#num v_LINK_MEASURE
#num v_LINK_STRENGTH
#num v_LINK_STRAIGHTNESS
#num v_LINK_EDGE
#num v_LINK_STRAIGHT
#num v_LINK_EDGE_POS
#num v_LINK_BOUNDARY

#starttype quad_tree
#field x           , CULong
#field y           , CULong
#field size        , CULong
#field level       , CULong
#field stat        , <statistics>
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
#field links       , <list>
#field context     , <typed_pointer>
#field annotation  , <typed_pointer>
#stoptype

#ccall quad_tree_destroy , Ptr <quad_tree> -> IO ()

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

#ccall quad_tree_ensure_edge_response , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> Ptr (Ptr <edge_response>) -> <truth_value> -> IO <result>

#ccall quad_tree_get_child_edge_response , Ptr <quad_forest> \
  -> Ptr <quad_tree> -> Ptr CDouble -> Ptr CDouble -> IO <result>

#ccall quad_tree_edge_response_to_line , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> Ptr <list> -> IO <result>

#ccall quad_tree_gradient_to_line , Ptr <quad_forest> -> Ptr <quad_tree> \
  -> Ptr <list> -> IO <result>

#ccall quad_tree_get_neighbors , Ptr <list> -> Ptr <quad_tree> -> IO <result>

#ccall quad_tree_link_equals , Ptr () -> Ptr () -> IO <truth_value>

#ccall quad_tree_find_link , Ptr <quad_tree> -> Ptr <quad_tree> \
  -> Ptr (Ptr <quad_tree_link_head>) -> IO <result>
