#include <bindings.dsl.h>
#include "cvsu_annotation.h"

module CVSU.Bindings.Annotation where

#strict_import

import CVSU.Bindings.Types
import Foreign.C.Types
import Foreign.Ptr

#starttype tree_annotation
#field data , <typed_pointer>
#stoptype


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
#field chain           , Ptr <quad_forest_edge_chain>
#field parent          , Ptr <quad_forest_edge>
#field prev            , Ptr <quad_forest_edge>
#field next            , Ptr <quad_forest_edge>
#field tree            , Ptr ()
#field length          , CULong
#field rank            , CULong
#field strength        , CDouble
#field dx              , CDouble
#field dy              , CDouble
#field mag             , CDouble
#field ang             , CDouble
#field mean            , CDouble
#field deviation       , CDouble
#field has_edge        , <truth_value>
#field is_intersection , <truth_value>
#field dir             , <direction>
#stoptype


#starttype quad_forest_intersection
#field tree   , Ptr <quad_tree>
#field edges  , <list>
#field chains , <list>
#stoptype
