#include <bindings.dsl.h>
#include "cvsu_types.h"

module CVSU.Bindings.Types where

#strict_import

import Foreign.Ptr

#integral_t truth_value

-- direction type and enum values

#integral_t direction

#num d_NULL
#num d_N
#num d_NE
#num d_E
#num d_SE
#num d_S
#num d_SW
#num d_W
#num d_NW
#num d_H
#num d_V
#num d_R
#num d_F
#num d_N4
#num d_N8

-- type_label type and enum values

#integral_t type_label

#num t_UNDEF
#num t_TYPE
#num t_TRUTH
#num t_POINTER
#num t_TPOINTER
#num t_STRING
#num t_S8
#num t_U8
#num t_S16
#num t_U16
#num t_S32
#num t_U32
#num t_S64
#num t_U64
#num t_F32
#num t_F64
#num t_SEGMENT
#num t_BOUNDARY
#num t_INTERSECTION
#num t_STAT_ACCUMULATOR
#num t_PATH_SNIFFER

#starttype typed_pointer
#field type  , <type_label>
#field count , CULong
#field value , Ptr ()
#stoptype

-- result type and enum values

#integral_t result

#num SUCCESS
#num FATAL
#num CAUGHT_ERROR
#num BAD_POINTER
#num BAD_TYPE
#num BAD_SIZE
#num BAD_PARAM
#num NOT_FOUND
#num NOT_IMPLEMENTED

-- pixel type and enum values

#integral_t pixel_type

#num p_NONE
#num p_U8
#num p_S8
#num p_U16
#num p_S16
#num p_U32
#num p_S32
-- #num p_U64
-- #num p_S64
#num p_F32
#num p_F64

#integral_t I_1_t
#integral_t I_2_t
#integral_t SI_1_t
#integral_t SI_2_t

-- pixel format and enum values

#integral_t pixel_format

#num NONE
#num MONO
#num GREY
#num UYVY
#num RGB
#num HSV
#num YUV
#num LAB
#num RGBA

-- image rect type

#starttype image_rect
#field valid  , CULong
#field offset , CULong
#field hstep  , CULong
#field vstep  , CULong
#field N      , CULong
#stoptype

-- point type

#starttype point
#field x , CShort
#field y , CShort
#stoptype

-- line type

#starttype line
#field start , <point>
#field end , <point>
#stoptype

-- weighted line type

#starttype weighted_line
#field start  , <point>
#field end    , <point>
#field weight , CDouble
#stoptype

--rect type

#starttype rect
#field left , <point>
#field right , <point>
#field top , <point>
#field bottom , <point>
#stoptype

-- statistics type

#starttype statistics
#field N , CDouble
#field sum , CDouble
#field sum2 , CDouble
#field mean , CDouble
#field variance , CDouble
#field deviation , CDouble
#stoptype
