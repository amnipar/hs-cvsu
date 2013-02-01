#include <bindings.dsl.h>
#include <cvsu_list.h>

module CVSU.Bindings.List where

#strict_import

import CVSU.Bindings.Types

import Foreign.Ptr
import Foreign.C.Types

#starttype list_item
#field prev , Ptr <list_item>
#field next , Ptr <list_item>
#field data , Ptr ()
#stoptype

#starttype chunk
#field item_size     , CULong
#field size          , CULong
#field count         , CULong
#field chunk_count   , CULong
#field current_chunk , CULong
#field chunks        , Ptr (Ptr CUChar)
#field chunk         , Ptr CUChar
#stoptype

#starttype list
#field parent     , Ptr <list>
#field first      , <list_item>
#field last       , <list_item>
#field first_free , <list_item>
#field last_free  , <list_item>
#field count      , CULong
#field max_size   , CULong
#field item_chunk , <chunk>
#field data_chunk , <chunk>
#stoptype

#ccall list_alloc , IO (Ptr <list>)

#ccall list_free , Ptr <list> -> IO ()

#ccall list_create , Ptr <list> -> CULong -> CULong -> CULong -> IO <result>

#ccall list_destroy , Ptr <list> -> IO <result>

#ccall list_nullify , Ptr <list> -> IO <result>

#ccall list_is_null , Ptr <list> -> IO <truth_value>

#ccall list_clear , Ptr <list> -> IO <result>

#ccall list_append , Ptr <list> -> Ptr () -> IO <result>

#ccall list_prepend , Ptr <list> -> Ptr () -> IO <result>
