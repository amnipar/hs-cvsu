#include <bindings.dsl.h>
#include "cvsu_parsing.h"

module CVSU.Bindings.Parsing where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.QuadForest
import CVSU.Bindings.List

import Foreign.C.Types
import Foreign.Ptr

-- #callback_t context_operation , Ptr <quad_forest> -> Ptr <quad_tree> \
--  -> Ptr <list> -> IO <result>

-- #ccall run_context_operation , Ptr <quad_forest> -> Ptr <list> -> Ptr <list> \
--  -> p'context_operation -> p'context_operation -> p'context_operation \
--  -> CULong -> <truth_value> -> IO <result>

#ccall quad_forest_calculate_accumulated_stats , Ptr <quad_forest> \
  -> CULong -> IO <result>

#ccall quad_forest_visualize_accumulated_stats , Ptr <quad_forest> \
  -> Ptr <pixel_image> -> IO <result>

#ccall quad_forest_calculate_neighborhood_stats , Ptr <quad_forest> \
  -> <truth_value> -> IO <result>

#integral_t stat_visualization_mode
#num v_STAT
#num v_NSTAT
#num v_OVERLAP
#num v_STRENGTH
#num v_SCORE

#ccall quad_forest_visualize_neighborhood_stats , Ptr <quad_forest> \
  -> Ptr <pixel_image> -> <stat_visualization_mode> -> IO <result>

#ccall quad_forest_calculate_edge_stats , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_parse , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_visualize_parse_result , Ptr <quad_forest> \
  -> Ptr <pixel_image> -> IO <result>
