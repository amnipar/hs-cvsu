#include <bindings.dsl.h>
#include "cvsu_quad_forest.h"

module CVSU.Bindings.QuadForest where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
--import CVSU.Bindings.Annotation
import CVSU.Bindings.PixelImage
import CVSU.Bindings.Integral
import CVSU.Bindings.List
--import CVSU.Bindings.QuadTree
import Foreign.C.Types
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
  -> Ptr (Ptr <segment>) -> IO <result>

#ccall quad_forest_get_segment_trees , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <segment>) -> CULong -> IO <result>

#ccall quad_forest_get_segment_neighbors , Ptr <list> -> Ptr <quad_forest> \
  -> Ptr (Ptr <segment>) -> CULong -> IO <result>

#ccall quad_forest_get_segment_mask , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <segment>) -> CULong -> <truth_value> -> IO <result>

#ccall quad_forest_get_segment_boundary , Ptr <quad_forest> \
  -> Ptr <segment> -> Ptr <list> -> IO <result>

#ccall quad_forest_get_links , Ptr <quad_forest> -> Ptr <list> \
  -> <link_visualization_mode> -> IO <result>

#ccall quad_forest_highlight_segments , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> Ptr (Ptr <segment>) -> CULong -> Ptr CUChar -> IO <result>

#ccall quad_forest_draw_image , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> <truth_value> -> IO <result>

#ccall quad_forest_draw_trees , Ptr <quad_forest> -> Ptr <pixel_image> \
  -> <truth_value> -> IO <result>

-- quad_tree structure and functions

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

#ccall quad_tree_link_destroy , Ptr <quad_tree_link> -> IO ()

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



-- accumulated stat type and related functions

#starttype accumulated_stat
#field meanmean , CDouble
#field meandev , CDouble
#field devmean , CDouble
#field devdev , CDouble
#field strength , CDouble
#stoptype

#ccall accumulated_stat_create , Ptr <quad_tree> -> Ptr <stat_accumulator> \
  -> IO <result>

#ccall ensure_accumulated_stat , Ptr <typed_pointer> \
  -> Ptr (Ptr <accumulated_stat>) -> IO <result>

#ccall is_accumulated_stat , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_accumulated_stat , Ptr <typed_pointer> -> IO (Ptr <accumulated_stat>)

#ccall expect_accumulated_stat , Ptr (Ptr <accumulated_stat>) \
  -> Ptr <typed_pointer> -> IO <result>

-- node_category type

#integral_t node_category
#num nc_UNDEF
#num nc_SEGMENT
#num nc_BOUNDARY
#num nc_CLUTTER

-- node hypothesis type

#starttype node_hypothesis
#field category , <node_category>
#field likelihood_score , CDouble
#field prev , Ptr <quad_tree>
#field next , Ptr <quad_tree>
#stoptype

-- neighborhood_stat structure and related functions

#starttype neighborhood_stat
#field mean_mean , CDouble
#field mean_dev , CDouble
#field dev_mean , CDouble
#field dev_dev , CDouble
#field strength , CDouble
#field overlap , CDouble
#field mean_ridge_score , CDouble
#field mean_ledge_score , CDouble
#field dev_ridge_score , CDouble
#field dev_ledge_score , CDouble
#field mag_ridge_score , CDouble
#field profile_score , CDouble
#field segment_score , CDouble
#field boundary_score , CDouble
#field dir_confusion , CDouble
#stoptype

#ccall ensure_neighborhood_stat , Ptr <typed_pointer> \
  -> Ptr (Ptr <neighborhood_stat>) -> IO <result>

#ccall is_neighborhood_stat , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_neighborhood_stat , Ptr <typed_pointer> \
  -> IO (Ptr <neighborhood_stat>)

#ccall expect_neighborhood_stat , Ptr (Ptr <neighborhood_stat>) \
  -> Ptr <typed_pointer> -> IO <result>

-- edge_response structure and related functions

#starttype edge_response
#field dx , CDouble
#field dy , CDouble
#field mag , CDouble
#field ang , CDouble
#field confidence , CDouble
#field x , CULong
#field y , CULong
#field hpeaks , CULong
#field vpeaks , CULong
#field peak_score , CDouble
#stoptype

#ccall ensure_edge_response , Ptr <typed_pointer> -> Ptr (Ptr <edge_response>) \
  -> IO <result>

#ccall is_edge_response , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_edge_response , Ptr <typed_pointer> -> CULong \
  -> IO (Ptr <edge_response>)

#ccall expect_edge_response , Ptr (Ptr <edge_response>) -> Ptr <typed_pointer> \
  -> IO <result>

-- link_measure structure and related functions

#integral_t link_category
#num bl_UNDEF
#num bl_TOWARDS
#num bl_AGAINST
#num bl_LEFT
#num bl_RIGHT
#num bl_PARALLEL
#num bl_PERPENDICULAR

#starttype link_measure
#field category , <link_category>
#field magnitude_score , CDouble
#field strength_score , CDouble
#field angle_score , CDouble
#field against_score , CDouble
#field profile_score , CDouble
#field parallel_score , CDouble
#field perpendicular_score , CDouble
#stoptype

#ccall ensure_link_measure , Ptr <typed_pointer> -> Ptr (Ptr <link_measure>) \
  -> CULong -> IO <result>

#ccall is_link_measure , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_link_measure , Ptr <typed_pointer> -> CULong \
  -> IO (Ptr <link_measure>)

#ccall expect_link_measure , Ptr <typed_pointer> -> Ptr (Ptr <link_measure>) \
  -> CULong -> IO <result>

-- edge_profile structure and related functions

#starttype edge_profile
#field mean_left , CDouble
#field mean_right , CDouble
#field dev_left , CDouble
#field dev_right , CDouble
#field mean_score , CDouble
#field dev_score , CDouble
#stoptype

#ccall is_edge_profile , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_edge_profile , Ptr <typed_pointer> -> CULong \
  -> IO (Ptr <edge_profile>)

-- edge_links structure and related functions

#starttype edge_links
#field towards , Ptr <quad_tree_link_head>
#field against , Ptr <quad_tree_link_head>
#field own_angle , CDouble
#field own_curvature , CDouble
#stoptype

#ccall ensure_edge_links , Ptr <typed_pointer> -> Ptr (Ptr <edge_links>) \
  -> CULong -> IO <result>

#ccall is_edge_links , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_edge_links , Ptr <typed_pointer> -> CULong -> IO (Ptr <edge_links>)

#ccall expect_edge_links , Ptr <typed_pointer> -> Ptr (Ptr <edge_links>) \
  -> CULong -> IO <result>

-- boundary_message structure and related functions

#starttype boundary_message
#field round , CULong
#field pool_curvature , CULong
#field acc_curvature , CULong
#field pool_distance , CULong
#field acc_distance , CULong
#field pool_length , CULong
#field acc_length , CULong
#stoptype

#ccall ensure_boundary_message , Ptr <typed_pointer> \
  -> Ptr (Ptr <boundary_message>) -> CULong -> IO <result>

#ccall is_boundary_message , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_boundary_message , Ptr <typed_pointer> -> CULong \
  -> IO (Ptr <boundary_message>)

-- segment_message structure and related structures

#starttype segment_message
#field extent , CULong
#field echo , <truth_value>
#field strength_diff , CDouble
#stoptype

#ccall ensure_segment_message , Ptr <typed_pointer> \
  -> Ptr (Ptr <segment_message>) -> CULong -> CDouble -> IO <result>

#ccall is_segment_message , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_segment_message , Ptr <typed_pointer> -> CULong \
  -> IO (Ptr <segment_message>)

#ccall expect_segment_message , Ptr <typed_pointer> \
  -> Ptr (Ptr <segment_message>) -> CULong -> IO <result>

-- boundary_category type

#integral_t boundary_category
#num fc_UNDEF
#num fc_UNKNOWN
#num fc_STRAIGHT
#num fc_CURVED
#num fc_CORNER
#num fc_INTERSECTION
#num fc_CLUTTER

-- fragment_model structure

#starttype fragment_model
#field x , CULong
#field y , CULong
#field length , CULong
#field angle , CDouble
#field curvature , CDouble
#field start_angle , CDouble
#field end_angle , CDouble
#field center , Ptr <boundary>
#field start , Ptr <boundary>
#field end , Ptr <boundary>
#stoptype

-- boundary structure and related functions

#starttype boundary
#field parent          , Ptr <boundary>
#field prev            , Ptr <boundary>
#field next            , Ptr <boundary>
#field first           , Ptr <boundary>
#field last            , Ptr <boundary>
#field first2          , Ptr <boundary>
#field last2           , Ptr <boundary>
#field tree            , Ptr <quad_tree>
#field category        , <boundary_category>
#field category2       , <boundary_category>
#field round           , CULong
#field rank            , CULong
#field length          , CULong
#field x               , CULong
#field y               , CULong
#field angle           , CDouble
#field dx              , CDouble
#field dy              , CDouble
#field dx1             , CDouble
#field dy1             , CDouble
#field dx2             , CDouble
#field dy2             , CDouble
#field smoothed_angle  , CDouble
#field cx              , CDouble
#field cy              , CDouble
#field curvature       , CDouble
#field quality         , CDouble
#field cx2             , CDouble
#field cy2             , CDouble
#field curvature2      , CDouble
#field quality2        , CDouble
#stoptype

-- boundary_info structure

#starttype boundary_info
#field parent , Ptr <boundary>
#field first , Ptr <boundary>
#field last , Ptr <boundary>
#field category , <boundary_category>
#field x1 , CULong
#field y1 , CULong
#field x2 , CULong
#field y2 , CULong
#field color[0] , Word8
#field color[1] , Word8
#field color[2] , Word8
#field color[3] , Word8
#field links , Ptr <list>
#field hypotheses , Ptr <list>
#stoptype

#ccall compare_boundaries_by_quality , Ptr () -> Ptr () -> IO CInt

#ccall compare_boundaries_by_length , Ptr () -> Ptr () -> IO CInt

#ccall is_boundary , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_boundary , Ptr <typed_pointer> -> CULong -> IO (Ptr <boundary)

#ccall quad_tree_ensure_boundary , Ptr <quad_tree> -> Ptr (Ptr <boundary>) \
  -> IO <result>

#ccall boundary_init , Ptr <boundary> -> Ptr <edge_links> -> IO ()

#ccall quad_tree_boundary_init , Ptr <quad_tree> -> Ptr (Ptr <boundary>) \
  -> Ptr <edge_links> -> IO <result>

#ccall boundary_union , Ptr <boundary> -> Ptr <boundary> -> IO ()

#ccall quad_tree_boundary_union , Ptr <quad_tree> -> Ptr <quad_tree> -> IO ()

#ccall boundary_find , Ptr <boundary> -> IO (Ptr <boundary>)

#ccall quad_tree_boundary_find , Ptr <quad_tree> -> IO (Ptr <boundary>)

#ccall quad_tree_boundary_id , Ptr <quad_tree> -> IO (CULong)

#ccall quad_tree_is_boundary_parent , Ptr <quad_tree> -> IO <truth_value>

-- segment_category type

#integral_t segment_category
#num sc_UNDEF
#num sc_FOREGROUND
#num sc_BACKGROUND
#num sc_CLUTTER

-- segment structure and related functions

#starttype segment
#field parent , Ptr <segment>
#field category , <segment_category>
#field rank , CULong
#field extent , CULong
#field stat , <statistics>
#stoptype

#starttype segment_info
#field center , Ptr <segment>
#field x1 , CULong
#field y1 , CULong
#field x2 , CULong
#field y2 , CULong
#field color[0] , Word8
#field color[1] , Word8
#field color[2] , Word8
#field color[3] , Word8
#field hypotheses , Ptr <list>
#stoptype

#ccall compare_segments , Ptr () -> Ptr () -> IO CInt

#ccall is_segment , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_segment , Ptr <typed_pointer> -> CULong -> IO (Ptr <segment>)

#ccall quad_tree_ensure_segment , Ptr <quad_tree> -> Ptr (Ptr <segment>) \
  -> IO <result>

#ccall quad_tree_get_segment , Ptr <quad_tree> -> IO (Ptr <segment>)

#ccall segment_union , Ptr <segment> -> Ptr <segment> -> IO ()

#ccall quad_tree_segment_union , Ptr <quad_tree> -> Ptr <quad_tree> -> IO ()

#ccall segment_find , Ptr <segment> -> IO (Ptr <segment>)

#ccall quad_tree_segment_find , Ptr <quad_tree> -> IO (Ptr <segment>)

#ccall quad_tree_segment_id , Ptr <quad_tree> -> IO CULong

#ccall quad_tree_is_segment_parent , Ptr <quad_tree> -> IO <truth_value>

-- object_hypothesis structure

#starttype object_hypothesis
#field class_id , CULong
#field extent , <uncertain_rect>
#field potential , CDouble
#stoptype

-- hypothesis_support structure

#starttype hypothesis_support
#field hypothesis , Ptr <object_hypothesis>
#field support , CDouble
#stoptype

-- stat_accumulator structure and functions

#starttype stat_accumulator
#field round      , CULong
#field mean_pool1 , CDouble
#field mean_acc1  , CDouble
#field mean_pool2 , CDouble
#field mean_acc2  , CDouble
#field dev_pool1  , CDouble
#field dev_acc1   , CDouble
#field dev_pool2  , CDouble
#field dev_acc2   , CDouble
#stoptype

#ccall is_stat_accumulator , Ptr <typed_pointer> -> IO <truth_value>

#ccall has_stat_accumulator , Ptr <typed_pointer> -> IO (Ptr <stat_accumulator>)

#ccall expect_stat_accumulator , Ptr (Ptr <stat_accumulator>) \
  -> Ptr <typed_pointer> -> IO <result>

#ccall ensure_stat_accumulator , Ptr <typed_pointer> \
  -> Ptr (Ptr <stat_accumulator>) -> IO <result>
