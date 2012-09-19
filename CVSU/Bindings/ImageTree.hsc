#include <bindings.dsl.h>
#include "cvsu_image_tree.h"

module CVSU.Bindings.ImageTree where

#strict_import

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.Integral
import CVSU.Bindings.List

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
-- import System.IO.Unsafe
-- import Data.Maybe

-- #starttype image_tree_neighbor
-- #field tree     , Ptr <image_tree>
-- #field strength , CFloat
-- #stoptype

#starttype image_tree
#field root   , Ptr <image_tree_root>
#field parent , Ptr <image_tree>
#field nw     , Ptr <image_tree>
#field ne     , Ptr <image_tree>
#field sw     , Ptr <image_tree>
#field se     , Ptr <image_tree>
#field block  , Ptr <image_block>
#field n      , Ptr <image_tree>
#field e      , Ptr <image_tree>
#field s      , Ptr <image_tree>
#field w      , Ptr <image_tree>
#field level  , CULong
#stoptype

#starttype image_tree_root
#field ROI    , <pixel_image>
#field I      , <integral_image>
#field box    , <small_integral_image_box>
#field forest , Ptr <image_tree_forest>
#field tree   , Ptr <image_tree>
#stoptype

#starttype image_tree_forest
#field original        , Ptr <pixel_image>
#field own_original    , CULong
#field rows            , CUShort
#field cols            , CUShort
#field tree_width      , CUShort
#field tree_height     , CUShort
#field dx              , CUShort
#field dy              , CUShort
#field type            , <image_block_type>
#field trees           , <list>
#field blocks          , <list>
#field last_base_tree  , Ptr <list_item>
#field last_base_block , Ptr <list_item>
#field roots           , Ptr <image_tree_root>
#stoptype


#ccall image_tree_forest_alloc , IO (Ptr <image_tree_forest>)

#ccall image_tree_forest_free , Ptr <image_tree_forest> -> IO ()

#ccall image_tree_forest_create , Ptr <image_tree_forest> -> \
  Ptr <pixel_image> -> CUShort -> CUShort -> IO <result>

#ccall image_tree_forest_reload , Ptr <image_tree_forest> -> \
  CUShort -> CUShort -> IO <result>
  
#ccall image_tree_forest_destroy , Ptr <image_tree_forest> -> IO <result>

#ccall image_tree_forest_nullify , Ptr <image_tree_forest> -> IO <result>

#ccall image_tree_forest_update_prepare , Ptr <image_tree_forest> -> IO <result>

#ccall image_tree_forest_update , Ptr <image_tree_forest> -> IO <result>

#ccall image_tree_forest_read , \
  Ptr <image_tree_forest> -> CString -> CUShort -> CUShort -> IO <result>

#ccall image_tree_root_update , Ptr <image_tree_root> -> IO <result>

#ccall image_tree_update , Ptr <image_tree> -> IO <result>

#ccall image_tree_divide , Ptr <image_tree> -> IO <result>

#ccall image_tree_create_neighbor_list , Ptr <list> -> IO <result>

#ccall image_tree_get_direct_neighbor , Ptr <image_tree> -> Ptr <image_tree> -> <direction> -> IO <result>

#ccall image_tree_get_direct_neighbor_n , Ptr <image_tree> -> Ptr <image_tree> -> IO <result>

#ccall image_tree_get_direct_neighbor_e , Ptr <image_tree> -> Ptr <image_tree> -> IO <result>

#ccall image_tree_get_direct_neighbor_s , Ptr <image_tree> -> Ptr <image_tree> -> IO <result>

#ccall image_tree_get_direct_neighbor_w , Ptr <image_tree> -> Ptr <image_tree> -> IO <result>

#ccall image_tree_find_all_immediate_neighbors , Ptr <list> -> Ptr <image_tree> -> IO <result>
