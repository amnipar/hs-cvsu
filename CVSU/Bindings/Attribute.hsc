#include <bindings.dsl.h>
#include <cvsu_attribute.h>

module CVSU.Bindings.Attribute where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer

import Foreign.Ptr
import Foreign.C.Types

-- attribute structure and functions

#starttype attribute
#field key   , CULong
#field value , <typed_pointer>
#stoptype

#ccall attribute_alloc , IO (Ptr <attribute>)

#ccall attribute_free , Ptr <attribute> -> IO ()

#ccall attribute_create , Ptr <attribute> -> CULong -> Ptr <typed_pointer> \
    -> IO (<result>)

#ccall attribute_destroy , Ptr <attribute> -> IO ()

#ccall attribute_nullify , Ptr <attribute> -> IO ()

#ccall attribute_is_null , Ptr <attribute> -> IO (<truth_value>)

-- attribute_list structure and functions

#starttype attribute_list
#field items , Ptr <attribute>
#field size  , CULong
#field count , CULong
#stoptype

#ccall attribute_list_alloc , IO (Ptr <attribute_list>)

#ccall attribute_list_free , Ptr <attribute_list> -> IO ()

#ccall attribute_list_create , Ptr <attribute_list> -> CULong -> IO (<result>)

#ccall attribute_list_destroy , Ptr <attribute_list> -> IO ()

#ccall attribute_list_nullify , Ptr <attribute_list> -> IO ()

#ccall attribute_list_is_null , Ptr <attribute_list> -> IO <truth_value>

#ccall attribute_add , Ptr <attribute_list> -> Ptr <attribute> \
    -> Ptr (Ptr <attribute>) -> IO (<result>)

#ccall attribute_find , Ptr <attribute_list> -> CULong -> IO (Ptr <attribute>)
