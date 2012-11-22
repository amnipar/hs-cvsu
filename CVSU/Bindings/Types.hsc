#include <bindings.dsl.h>
#include "cvsu_types.h"

module CVSU.Bindings.Types where

#strict_import

import Foreign.Ptr

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
#num p_U64
#num p_S64
#num p_F32
#num p_F64

#integral_t I_1_t
#integral_t I_2_t
#integral_t SI_1_t
#integral_t SI_2_t

-- pixel format and enum values

#integral_t pixel_format

#num NONE
#num GREY
#num UYVY
#num RGB
#num HSV
#num YUV
#num LAB
#num RGBA

-- block type and enum values

#integral_t image_block_type

#num b_NONE
#num b_INT
#num b_REAL
#num b_STAT_GREY
#num b_STAT_COLOR
#num b_STAT_WITH_DIR
#num b_HSTAT_GREY
#num b_HSTAT_COLOR

-- image block type

#starttype image_block
#field x     , CUShort
#field y     , CUShort
#field w     , CUShort
#field h     , CUShort
#field value , Ptr ()
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

--rect type

#starttype rect
#field left , <point>
#field right , <point>
#field top , <point>
#field bottom , <point>
#stoptype

-- dir type

#starttype dir
#field h , CShort
#field v , CShort
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

-- stat grey type

#starttype stat_grey
#field mean , CShort
#field dev  , CShort
#stoptype

-- stat color type

#starttype stat_color
#field mean_i , CShort
#field dev_i  , CShort
#field mean_c1 , CShort
#field dev_c1  , CShort
#field mean_c2 , CShort
#field dev_c2  , CShort
#stoptype

#starttype stat_with_dir
#field mean   , CShort
#field dev    , CShort
#field dir_h  , CShort
#field dir_v  , CShort
#stoptype

#starttype hstat_grey
#field mean    , CShort
#field dev     , CShort
#field mean_nw , CShort
#field mean_ne , CShort
#field mean_se , CShort
#field mean_sw , CShort
#stoptype

#starttype hstat_color
#field mean_i     , CShort
#field dev_i      , CShort
#field mean_c1    , CShort
#field dev_c1     , CShort
#field mean_c2    , CShort
#field dev_c2     , CShort
#field mean_i_nw  , CShort
#field mean_c1_nw , CShort
#field mean_c2_nw , CShort
#field mean_i_ne  , CShort
#field mean_c1_ne , CShort
#field mean_c2_ne , CShort
#field mean_i_se  , CShort
#field mean_c1_se , CShort
#field mean_c2_se , CShort
#field mean_i_sw  , CShort
#field mean_c1_sw , CShort
#field mean_c2_sw , CShort
#stoptype
