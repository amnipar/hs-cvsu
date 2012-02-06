#include <bindings.dsl.h>
#include "cvsu_integral.h"

module CVSU.Bindings.Integral where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

import Foreign.Ptr

#starttype integral_image
#field original , Ptr <pixel_image>
#field I_1      , <pixel_image>
#field I_2      , <pixel_image>
#field width    , CULong
#field height   , CULong
#field step     , CULong
#field stride   , CULong
#stoptype

#starttype small_integral_image_box
#field I_1_data , Ptr <SI_1_t>
#field iA       , Ptr <SI_1_t>
#field sum      , <SI_1_t>
#field I_2_data , Ptr <SI_2_t>
#field i2A      , Ptr <SI_2_t>
#field sumsqr   , <SI_2_t>
#field offset   , CULong
#field B_inc    , CULong
#field C_inc    , CULong
#field D_inc    , CULong
#field N        , CULong
#field stride   , CULong
#field dx       , CULong
#field dy       , CULong
#stoptype
