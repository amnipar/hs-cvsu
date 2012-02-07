module CVSU.PixelImage
( PixelImage(..)
, PixelType(..)
, PixelFormat(..)
, formatToStep
, formatToStride
, allocPixelImage
, createPixelImage
, createPixelImageFromData
, convertPixelImage
, ptrToPixelImage
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Maybe

data PixelType = U8 | S8 | U16 | S16 | U32 | S32 | U64 | S64 | F32 | F64 deriving (Eq, Show)

cPixelType :: PixelType -> C'pixel_type
cPixelType t =
  case t of
    U8  -> c'p_U8
    S8  -> c'p_S8
    U16 -> c'p_U16
    S16 -> c'p_S16
    U32 -> c'p_U32
    S32 -> c'p_S32
    U64 -> c'p_U64
    S64 -> c'p_S64
    F32 -> c'p_F32
    F64 -> c'p_F64

hPixelType :: C'pixel_type -> PixelType
hPixelType t =
  case t of
    c'p_U8   -> U8
    c'p_S8   -> S8
    c'p_U16  -> U16
    c'p_S16  -> S16
    c'p_U32  -> U32
    c'p_S32  -> S32
    c'p_U64  -> U64
    c'p_S64  -> S64
    c'p_F32  -> F32
    c'p_F64  -> F64
    --c'p_NONE -> U8

data PixelFormat = Grey | UYVY | RGB | BGR | HSV | YUV | LAB | RGBA deriving (Eq, Show)

cPixelFormat :: PixelFormat -> C'pixel_format
cPixelFormat f =
  case f of
    Grey -> c'GREY
    UYVY -> c'UYVY
    RGB  -> c'RGB
    -- BGR  -> c'BGR
    HSV  -> c'HSV
    YUV  -> c'YUV
    LAB  -> c'LAB
    RGBA -> c'RGBA
    _    -> c'NONE

hPixelFormat :: C'pixel_format -> PixelFormat
hPixelFormat f =
  case f of
    c'GREY -> Grey
    c'UYVY -> UYVY
    c'RGB  -> RGB
    -- c'BGR  -> BGR
    c'HSV  -> HSV
    c'YUV  -> YUV
    c'LAB  -> LAB
    c'RGBA -> RGBA
    -- c'NONE -> GREY

formatToStep :: PixelFormat -> Int
formatToStep f =
  case f of
    Grey -> 1
    UYVY -> 2
    RGB  -> 3
    BGR  -> 3
    HSV  -> 3
    YUV  -> 3
    LAB  -> 3
    RGBA -> 4

formatToStride :: Int -> PixelFormat -> Int
formatToStride w f = w * formatToStep f

data PixelImage =
  NullImage |
  PixelImage
  { imagePtr :: !(ForeignPtr C'pixel_image)
  , pixelType :: PixelType
  , pixelFormat :: PixelFormat
  , width :: Int
  , height :: Int
  } deriving Eq

allocPixelImage :: IO (Maybe (ForeignPtr C'pixel_image))
allocPixelImage = do
  ptr <- c'pixel_image_alloc
  if ptr /= nullPtr
       then do
         foreignPtr <- newForeignPtr p'pixel_image_free ptr
         return $ Just foreignPtr
         else do
           return Nothing

createPixelImage :: PixelType -> PixelFormat -> Int -> Int -> IO (PixelImage)
createPixelImage t f w h = do
  img <- allocPixelImage
  if isNothing img
    then do
      return NullImage
    else do
      withForeignPtr (fromJust img) $ \img_ptr -> do
        r <- c'pixel_image_create img_ptr
            (cPixelType t)
            (cPixelFormat f)
            (fromIntegral w)
            (fromIntegral h)
            (fromIntegral $ formatToStep f)
            (fromIntegral $ formatToStride w f)
        if r /= c'SUCCESS
          then do
            return NullImage
          else do
            return $ PixelImage (fromJust img) t f w h

createPixelImageFromData :: PixelType -> PixelFormat -> Int -> Int -> Ptr () -> IO (PixelImage)
createPixelImageFromData t f w h d = do
  img <- allocPixelImage
  if isNothing img
    then do
      return NullImage
    else do
      withForeignPtr (fromJust img) $ \img_ptr -> do
        r <- c'pixel_image_create_from_data img_ptr d
            (cPixelType t)
            (cPixelFormat f)
            (fromIntegral w)
            (fromIntegral h)
            (fromIntegral $ formatToStep f)
            (fromIntegral $ formatToStride w f)
        if r /= c'SUCCESS
          then do
            return NullImage
          else do
            return $ PixelImage (fromJust img) t f w h

convertPixelImage :: PixelImage -> PixelImage -> IO (PixelImage)
convertPixelImage src dst = do
  withForeignPtr (imagePtr src) $ \src_img -> do
    withForeignPtr (imagePtr dst) $ \dst_img -> do
      r <- c'pixel_image_convert src_img dst_img
      if r /= c'SUCCESS
        then do
          return NullImage
        else do
          return $ dst

ptrToPixelImage :: Ptr C'pixel_image -> IO (PixelImage)
ptrToPixelImage ptr
  | ptr == nullPtr = return NullImage
  | otherwise      = do
    C'pixel_image{
    c'pixel_image'type = t,
    c'pixel_image'format = f,
    c'pixel_image'width = w,
    c'pixel_image'height = h
    } <- peek ptr
    poke (p'pixel_image'own_data ptr) 0
    foreignPtr <- newForeignPtr p'pixel_image_free ptr
    return $ PixelImage foreignPtr
      (hPixelType t)
      (hPixelFormat f)
      (fromIntegral w)
      (fromIntegral h)
