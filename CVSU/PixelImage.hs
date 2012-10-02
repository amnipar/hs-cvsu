module CVSU.PixelImage
( PixelImage(..)
, PixelType(..)
, PixelFormat(..)
, valueConverter
, formatToStep
, formatToStride
, allocPixelImage
, createPixelImage
, readPixelImage
, createPixelImageFromData
, convertPixelImage
, withPixelImage
, ptrToPixelImage
, getAllPixels
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.OpenCV

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad
import Data.Maybe

import Debug.Trace

data PixelType =
  U8  |
  S8  |
  U16 |
  S16 |
  U32 |
  S32 |
  U64 |
  S64 |
  F32 |
  F64
  deriving (Eq, Show)

cPixelType :: PixelType -> C'pixel_type
cPixelType t
  | t == U8  = c'p_U8
  | t == S8  = c'p_S8
  | t == U16 = c'p_U16
  | t == S16 = c'p_S16
  | t == U32 = c'p_U32
  | t == S32 = c'p_S32
  | t == U64 = c'p_U64
  | t == S64 = c'p_S64
  | t == F32 = c'p_F32
  | t == F64 = c'p_F64

hPixelType :: C'pixel_type -> PixelType
hPixelType t
  | t == c'p_U8  = U8
  | t == c'p_S8  = S8
  | t == c'p_U16 = U16
  | t == c'p_S16 = S16
  | t == c'p_U32 = U32
  | t == c'p_S32 = S32
  | t == c'p_U64 = U64
  | t == c'p_S64 = S64
  | t == c'p_F32 = F32
  | t == c'p_F64 = F64
  | otherwise    = U8

valueConverter :: PixelType -> (Ptr() -> Int -> IO Float)
valueConverter t =
  case t of
    U8  -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CUChar) o)))
    S8  -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CSChar) o)))
    U16 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CUShort) o)))
    S16 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CShort) o)))
    U32 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CULong) o)))
    S32 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CLong) o)))
    U64 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CLLong) o)))
    S64 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CULLong) o)))
    F32 -> (\p o -> (liftM realToFrac $ peek (advancePtr ((castPtr p)::Ptr CFloat) o)))
    F64 -> (\p o -> (liftM realToFrac $ peek (advancePtr ((castPtr p)::Ptr CDouble) o)))

data PixelFormat =
  FormatNone |
  FormatGrey |
  FormatUYVY |
  FormatRGB |
  FormatBGR |
  FormatHSV |
  FormatYUV |
  FormatLAB |
  FormatRGBA
  deriving (Eq, Show)

cPixelFormat :: PixelFormat -> C'pixel_format
cPixelFormat f =
  case f of
    FormatGrey -> c'GREY
    FormatUYVY -> c'UYVY
    FormatRGB  -> c'RGB
    -- FormatBGR  -> c'BGR
    FormatHSV  -> c'HSV
    FormatYUV  -> c'YUV
    FormatLAB  -> c'LAB
    FormatRGBA -> c'RGBA
    _         -> c'NONE

hPixelFormat :: C'pixel_format -> PixelFormat
hPixelFormat f =
  case f of
    c'GREY -> FormatGrey
    c'UYVY -> FormatUYVY
    c'RGB  -> FormatRGB
    -- c'BGR  -> FormatBGR
    c'HSV  -> FormatHSV
    c'YUV  -> FormatYUV
    c'LAB  -> FormatLAB
    c'RGBA -> FormatRGBA
    _      -> FormatNone

formatToStep :: PixelFormat -> Int
formatToStep f =
  case f of
    FormatGrey -> 1
    FormatUYVY -> 2
    FormatRGB  -> 3
    FormatBGR  -> 3
    FormatHSV  -> 3
    FormatYUV  -> 3
    FormatLAB  -> 3
    FormatRGBA -> 4
    _          -> 0

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
  , dx :: Int
  , dy :: Int
  , stride :: Int
  , d :: Ptr()
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
    then return NullImage
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
          then return NullImage
          else ptrToPixelImage True (fromJust img)

readPixelImage :: String -> IO (PixelImage)
readPixelImage f = do
  img <- allocPixelImage
  if isNothing img
    then return NullImage
    else do
      withForeignPtr (fromJust img) $ \img_ptr ->
        withCString f $ \c_str -> do
          r <- c'pixel_image_create_from_file img_ptr c_str c'p_U8 c'GREY
          if r /= c'SUCCESS
            then return NullImage
            else ptrToPixelImage True (fromJust img)

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
          then return NullImage
          else ptrToPixelImage False (fromJust img)

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

withPixelImage :: PixelImage -> (PixelImage -> b) -> IO b
withPixelImage pimg op =
  withForeignPtr (imagePtr pimg) $ \iptr -> do
    r <- return $! op pimg
    touchForeignPtr $ imagePtr pimg
    return r

ptrToPixelImage :: Bool -> ForeignPtr C'pixel_image -> IO (PixelImage)
ptrToPixelImage ownsData fptr =
  withForeignPtr fptr $ \iptr ->
    if iptr == nullPtr
      then return NullImage
      else do
        C'pixel_image{
        c'pixel_image'data = d,
        c'pixel_image'type = t,
        c'pixel_image'format = f,
        c'pixel_image'width = w,
        c'pixel_image'height = h,
        c'pixel_image'dx = dx,
        c'pixel_image'dy = dy,
        c'pixel_image'stride = stride
        } <- peek iptr
        setOwnership ownsData iptr
        return $ PixelImage fptr
          (hPixelType t)
          (hPixelFormat f)
          (fromIntegral w)
          (fromIntegral h)
          (fromIntegral dx)
          (fromIntegral dy)
          (fromIntegral stride)
          d
  where
    setOwnership owns ptr
      | owns == True = poke (p'pixel_image'own_data ptr) 0
      | otherwise    = return ()

getAllPixels :: PixelImage -> IO [((Int,Int),Float)]
getAllPixels (PixelImage ptr t _ w h dx dy s d) =
  mapM getV [(x,y,(dy+y)*s+dx+x) | x <- [0..w-1], y <- [0..h-1]]
  where
        getV (x,y,o) = do
          v <- vc d o
          return ((x,y),v)
        vc = valueConverter t
