#include <bindings.dsl.h>
#include <cvsu_graph.h>

module CVSU.Bindings.Graph where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import CVSU.Bindings.List
import CVSU.Bindings.PixelImage

import Foreign.Ptr
import Foreign.C.Types

-- attribute type and functions

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

-- attribute_list type and functions

#starttype attribute_list
#field items , Ptr <attribute>
#field count , CULong
#stoptype

#ccall attribute_list_alloc , IO (Ptr <attribute_list>)

#ccall attribute_list_free , Ptr <attribute_list> -> IO ()

#ccall attribute_list_create , Ptr <attribute_list> -> CULong -> IO (<result>)

#ccall attribute_list_destroy , Ptr <attribute_list> -> IO ()

#ccall attribute_list_nullify , Ptr <attribute_list> -> IO ()

#ccall attribute_list_is_null , Ptr <attribute_list> -> IO <truth_value>

#ccall attribute_add , Ptr <attribute_list> -> Ptr <attribute> -> IO (<result>)

#ccall attribute_find , Ptr <attribute_list> -> CULong -> IO (Ptr <attribute>)

#starttype node
#field x           , CDouble
#field y           , CDouble
#field orientation , CDouble
#field scale       , CULong
#field attributes  , <attribute_list>
#field links       , <attribute_list>
#field bonds       , <attribute_list>
#stoptype

#starttype link
#field a          , <link_head>
#field b          , <link_head>
#field weight     , CDouble
#field attributes , <attribute_list>
#stoptype

#starttype link_head
#field body       , Ptr <link>
#field other      , Ptr <link_head>
#field origin     , Ptr <node>
#field attributes , <attribute_list>
#stoptype

#starttype graph
#field nodes   , <list>
#field links   , <list>
#field sources , <attribute_list>
#stoptype

#integral_t graph_neighborhood
#num NEIGHBORHOOD_0
#num NEIGHBORHOOD_4
#num NEIGHBORHOOD_6
#num NEIGHBORHOOD_8

#ccall graph_alloc , IO (Ptr <graph>)

#ccall graph_free , Ptr <graph> -> IO ()

#ccall graph_create , Ptr <graph> -> CULong -> CULong -> Ptr <attribute>
    -> IO <result>

#ccall graph_destroy , Ptr <graph> -> IO ()

#ccall graph_nullify , Ptr <graph> -> IO ()

#ccall graph_is_null , Ptr <graph> -> IO (<truth_value>)

#ccall graph_create_from_image , Ptr <graph> -> Ptr <pixel_image> -> CULong \
    -> CULong -> CULong -> CULong -> <graph_neighborhood> -> Ptr <attribute> \
    -> IO <result>
