#include <bindings.dsl.h>
#include "cvsu_segmentation.h"

module CVSU.Bindings.Segmentation where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.QuadForest
import Foreign.C.Types
import Foreign.Ptr

#ccall quad_forest_refresh_segments , Ptr <quad_forest> -> IO <result>

#ccall quad_forest_segment_with_deviation , Ptr <quad_forest> -> CDouble \
  -> CDouble -> IO <result>

#ccall quad_forest_segment_with_overlap , Ptr <quad_forest> -> CDouble \
  -> CDouble -> CDouble -> IO <result>
