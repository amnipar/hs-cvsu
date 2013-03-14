#include <bindings.dsl.h>
#include "cvsu_context.h"

module CVSU.Bindings.Context where

#strict_import

import CVSU.Bindings.Types
import Foreign.C.Types
import Foreign.Ptr



#starttype parse_context
#field token , CULong
#field round , CULong
#field data  , <typed_pointer>
#stoptype
