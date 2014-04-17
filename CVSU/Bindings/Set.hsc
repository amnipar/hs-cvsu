#include <bindings.dsl.h>
#include <cvsu_set.h>

module CVSU.Bindings.Set where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.Attribute

import Foreign.Ptr
import Foreign.C.Types

-- disjoint_set structure and functions

#starttype disjoint_set
#field id , Ptr <disjoint_set>
#field rank , CULong
#field size , CULong
#field attributes , <attribute_list>
#stoptype

#ccall disjoint_set_alloc , IO (Ptr <disjoint_set>)

#ccall disjoint_set_free , Ptr <disjoint_set> -> IO ()

#ccall disjoint_set_nullify , Ptr <disjoint_set> -> IO ()

#ccall disjoint_set_is_null , Ptr <disjoint_set> -> IO (<truth_value>)

#ccall disjoint_set_create , Ptr <disjoint_set> -> CULong -> IO (<result>)

#ccall disjoint_set_destroy , Ptr <disjoint_set> -> IO ()

#ccall disjoint_set_union , Ptr <disjoint_set> -> Ptr <disjoint_set> \
  -> IO (Ptr <disjoint_set>)

#ccall disjoint_set_find , Ptr <disjoint_set> -> IO (Ptr <disjoint_set>)

#ccall disjoint_set_id , Ptr <disjoint_set> -> IO (CULong)
