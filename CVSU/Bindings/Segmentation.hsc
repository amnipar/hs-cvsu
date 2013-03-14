#include <bindings.dsl.h>
#include "cvsu_segmentation.h"

module CVSU.Bindings.Segmentation where

#strict_import

import CVSU.Bindings.Types
import Foreign.C.Types
import Foreign.Ptr


#ccall quad_forest_segment_with_deviation , Ptr <quad_forest> -> CDouble \
  -> CDouble -> IO <result>

#ccall quad_forest_segment_with_overlap , Ptr <quad_forest> -> CDouble \
  -> CDouble -> CDouble -> IO <result>

#ccall quad_forest_segment_edges , Ptr <quad_forest> -> CULong \
  -> CDouble -> <direction> -> CULong -> CDouble -> <direction> -> <direction> \
  -> IO <result>

#ccall quad_forest_segment_with_boundaries , Ptr <quad_forest> -> CULong \
  -> CDouble -> CDouble -> CDouble -> CDouble -> <truth_value> \
  -> <truth_value> -> IO <result>
