#include <bindings.dsl.h>
#include "cvsu_context.h"

module CVSU.Bindings.Context where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import Foreign.C.Types
import Foreign.Ptr

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
