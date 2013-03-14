#include <bindings.dsl.h>
#include "cvsu_parsing.h"

module CVSU.Bindings.Parsing where

#strict_import

import CVSU.Bindings.Types
import Foreign.C.Types
import Foreign.Ptr

#ccall quad_forest_parse , Ptr <quad_forest> -> CULong -> CDouble \
  -> CULong -> IO <result>
