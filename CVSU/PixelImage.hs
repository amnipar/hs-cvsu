module CVSU.PixelImage
( PixelImage(..)
, PixelType(..)
, PixelFormat(..)
, valueConverter
, formatToStep
, formatToStride
, allocPixelImage
, createPixelImage
, readPNMPixelImage
, writePNMPixelImage
, readPixelImage
, createPixelImageFromData
, convertPixelImage
, withPixelImage
, ptrToPixelImage
, getPixel
, getAllPixels
, imageMinByRect
, imageMaxByRect
, imageMeanByRect
, imageVarianceByRect
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.OpenCV

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Concurrent
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
  -- U64 |
  -- S64 |
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
--  | t == U64 = c'p_U64
--  | t == S64 = c'p_S64
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
--  | t == c'p_U64 = U64
--  | t == c'p_S64 = S64
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
--    U64 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CLLong) o)))
--    S64 -> (\p o -> (liftM fromIntegral $ peek (advancePtr ((castPtr p)::Ptr CULLong) o)))
    F32 -> (\p o -> (liftM realToFrac $ peek (advancePtr ((castPtr p)::Ptr CFloat) o)))
    F64 -> (\p o -> (liftM realToFrac $ peek (advancePtr ((castPtr p)::Ptr CDouble) o)))

data PixelFormat =
  FormatNone |
  FormatMono |
  FormatGrey |
  FormatUYVY |
  FormatRGB |
--  FormatBGR |
  FormatHSV |
  FormatYUV |
  FormatLAB |
  FormatRGBA
  deriving (Eq, Show)

cPixelFormat :: PixelFormat -> C'pixel_format
cPixelFormat f =
  case f of
    FormatMono -> c'MONO
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
hPixelFormat f
  | f == c'MONO = FormatMono
  | f == c'GREY = FormatGrey
  | f == c'UYVY = FormatUYVY
  | f == c'RGB  = FormatRGB
--  | f == c'BGR  -> FormatBGR
  | f == c'HSV  = FormatHSV
  | f == c'YUV  = FormatYUV
  | f == c'LAB  = FormatLAB
  | f == c'RGBA = FormatRGBA
  | otherwise   = FormatNone

formatToStep :: PixelFormat -> Int
formatToStep f =
  case f of
    FormatMono -> 1
    FormatGrey -> 1
    FormatUYVY -> 2
    FormatRGB  -> 3
--    FormatBGR  -> 3
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

allocPixelImage :: IO (ForeignPtr C'pixel_image)
allocPixelImage = do
  ptr <- c'pixel_image_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'pixel_image_free ptr)
    else error "Memory allocation failed in allocPixelImage"

createPixelImage :: PixelType -> PixelFormat -> Int -> Int -> IO (PixelImage)
createPixelImage t f w h = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    r <- c'pixel_image_create pimg
      (cPixelType t)
      (cPixelFormat f)
      (fromIntegral w)
      (fromIntegral h)
      (fromIntegral $ formatToStep f)
      (fromIntegral $ formatToStride w f)
    if r /= c'SUCCESS
      then return NullImage
      else ptrToPixelImage fimg

readPNMPixelImage :: String -> IO (PixelImage)
readPNMPixelImage filename = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withCString filename $ \pfilename -> do
      r <- c'pixel_image_read pimg pfilename
      if r /= c'SUCCESS
        then error $ "Failed to read image " ++ filename
        else ptrToPixelImage fimg

writePNMPixelImage :: String -> Bool -> PixelImage -> IO ()
writePNMPixelImage filename ascii img =
  withForeignPtr (imagePtr img) $ \pimg ->
    withCString filename $ \pfilename -> do
      r <- c'pixel_image_write pimg pfilename (fromIntegral $ if ascii then 1 else 0)
      if r /= c'SUCCESS
        then error $ "Failed to write image " ++ filename
        else return ()


readPixelImage :: String -> IO (PixelImage)
readPixelImage f = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withCString f $ \c_str -> do
      r <- c'pixel_image_create_from_file pimg c_str c'p_U8 c'GREY
      if r /= c'SUCCESS
        then return NullImage
        else ptrToPixelImage fimg

createPixelImageFromData :: PixelType -> PixelFormat -> Int -> Int -> Ptr () -> IO (PixelImage)
createPixelImageFromData t f w h d = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    r <- c'pixel_image_create_from_data pimg d
      (cPixelType t)
      (cPixelFormat f)
      (fromIntegral w)
      (fromIntegral h)
      (fromIntegral $ formatToStep f)
      (fromIntegral $ formatToStride w f)
    if r /= c'SUCCESS
      then return NullImage
      else ptrToPixelImage fimg

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

ptrToPixelImage :: ForeignPtr C'pixel_image -> IO (PixelImage)
ptrToPixelImage fptr =
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
        return $ PixelImage fptr
          (hPixelType t)
          (hPixelFormat f)
          (fromIntegral w)
          (fromIntegral h)
          (fromIntegral dx)
          (fromIntegral dy)
          (fromIntegral stride)
          d

getPixel :: PixelImage -> (Int,Int) -> IO (Float)
getPixel (PixelImage ptr t _ _ _ dx dy s d) (x,y) =
  valueConverter t d ((dy+y)*s+dx+x)

getAllPixels :: PixelImage -> IO [((Int,Int),Float)]
getAllPixels (PixelImage ptr t _ w h dx dy s d) =
  mapM getV [(x,y,(dy+y)*s+dx+x) | x <- [0..w-1], y <- [0..h-1]]
  where
    getV (x,y,o) = do
      v <- valueConverter t d o
      return ((x,y),v)

imageMinByRect :: PixelImage -> (Int,Int) -> (Int,Int) -> IO (Double)
imageMinByRect img (x,y) (w,h) =
  withForeignPtr (imagePtr img) $ \pimg ->
    liftM realToFrac $ c'pixel_image_find_min_byte pimg
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0

imageMaxByRect :: PixelImage -> (Int,Int) -> (Int,Int) -> IO (Double)
imageMaxByRect img (x,y) (w,h) =
  withForeignPtr (imagePtr img) $ \pimg ->
    liftM realToFrac $ c'pixel_image_find_max_byte pimg
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0

imageMeanByRect :: PixelImage -> (Int,Int) -> (Int,Int) -> IO (Double)
imageMeanByRect img (x,y) (w,h) =
  withForeignPtr (imagePtr img) $ \pimg ->
    liftM realToFrac $ c'pixel_image_calculate_mean_byte pimg
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0

imageVarianceByRect :: PixelImage -> (Int,Int) -> (Int,Int) -> IO (Double)
imageVarianceByRect img (x,y) (w,h) =
  withForeignPtr (imagePtr img) $ \pimg ->
    liftM realToFrac $ c'pixel_image_calculate_variance_byte pimg
      (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0
