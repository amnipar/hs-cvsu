-- | Export some c symbols required for implementing conversion functions.
module CVSU.C.Types
( CResult
, resultSuccess
, CPixelImage
, CPixelFormat
, formatMono
, formatGrey
, formatRGB
, CPixelType
, typeByte
, typeFloat
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

type CResult      = C'result
type CPixelImage  = C'pixel_image
type CPixelFormat = C'pixel_format
type CPixelType   = C'pixel_type

resultSuccess :: CResult
resultSuccess = c'SUCCESS

formatMono :: CPixelFormat
formatMono = c'MONO

formatGrey :: CPixelFormat
formatGrey = c'GREY

formatRGB :: CPixelFormat
formatRGB = c'RGB

typeByte :: CPixelType
typeByte = c'p_U8

typeFloat :: CPixelType
typeFloat = c'p_F32
