#include <bindings.dsl.h>
#include "cvsu_typed_pointer.h"

module CVSU.Bindings.TypedPointer where

#strict_import

import CVSU.Bindings.Types
import Foreign.C.Types
import Foreign.Ptr

#integral_t type_label
#num t_UNDEF
#num t_type
#num t_truth_value
#num t_pointer
#num t_typed_pointer
#num t_string
#num t_S8
#num t_U8
#num t_S16
#num t_U16
#num t_S32
#num t_U32
#num t_F32
#num t_F64
#num t_tuple
#num t_list
#num t_disjoint_set
#num t_graph
#num t_node
#num t_attribute
#num t_attribute_list
#num t_link
#num t_link_head
#num t_statistics
#num t_accumulated_stat
#num t_neighborhood_stat
#num t_edge_response
#num t_link_measure
#num t_edge_profile
#num t_edge_links
#num t_boundary_message
#num t_boundary
#num t_segment_message
#num t_segment
#num t_stat_accumulator
#num t_pixel_image

#starttype typed_pointer
#field type , <type_label>
#field count , CULong
#field token , CULong
#field value , Ptr ()
#stoptype

#ccall typed_pointer_alloc , IO (Ptr <typed_pointer>)

#ccall typed_pointer_free , Ptr <typed_pointer> -> IO ()

#ccall typed_pointer_create , Ptr <typed_pointer> -> <type_label> -> CULong \
    -> CULong -> Ptr () -> IO (<result>)

#ccall typed_pointer_destroy , Ptr <typed_pointer> -> IO ()

#ccall typed_pointer_nullify , Ptr <typed_pointer> -> IO ()

#ccall typed_pointer_is_null , Ptr <typed_pointer> -> IO <truth_value>

#ccall is_typed_pointer , Ptr <typed_pointer> -> IO <truth_value>

#ccall typed_pointer_clone , Ptr <typed_pointer> -> Ptr <typed_pointer> \
    -> IO (<result>)

#ccall typed_pointer_copy , Ptr <typed_pointer> -> Ptr <typed_pointer> \
    -> IO (<result>)

#ccall typed_pointer_set_value , Ptr <typed_pointer> -> CULong -> Ptr () \
    -> IO (<result>)

#ccall tuple_create , Ptr <typed_pointer> -> CULong -> IO <result>

#ccall tuple_destroy , Ptr <typed_pointer> -> IO ()

#ccall tuple_promote , Ptr <typed_pointer> -> IO <result>

#ccall tuple_extend , Ptr <typed_pointer> -> Ptr <typed_pointer> \
  -> Ptr (Ptr <typed_pointer>) -> IO <result>

#ccall tuple_ensure_has_unique , Ptr <typed_pointer> -> <type_label> \
  -> Ptr (Ptr <typed_pointer>) -> IO <result>

#ccall tuple_has_type , Ptr <typed_pointer> -> <type_label> \
  -> IO (Ptr <typed_pointer>)

#ccall is_tuple , Ptr <typed_pointer> -> <truth_value>

#ccall ensure_has , Ptr <typed_pointer> -> <type_label> \
  -> Ptr (Ptr <typed_pointer>) -> IO <result>

#ccall ensure_is , Ptr <typed_pointer> -> <type_label> \
  -> Ptr (Ptr <typed_pointer>) -> IO <result>
