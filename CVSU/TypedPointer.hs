module CVSU.TypedPointer
( PointableType(..)
, showPointableType
, typedPointerNull
, typedPointerCreate
, Pointable(..)
) where

import CVSU.Bindings.TypedPointer
import CVSU.Bindings.Types

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Concurrent

import Control.Monad (liftM)

data PointableType =
  PUndef |
  PType |
  PTruth |
  PPointer |
  PPointable |
  PString |
  PCSChar |
  PCUChar |
  PCShort |
  PCUShort |
  PCInt |
  PCUInt |
  PCFloat |
  PCDouble |
  PTuple |
  PList |
  PSet |
  PGraph |
  PNode |
  PAttribute |
  PAttributeList |
  PLink |
  PLinkHead |
  PStatistics |
  PRawMoments |
  PPixelImage deriving (Eq,Show)

cTypeLabel :: PointableType -> C'type_label
cTypeLabel l
  | l == PUndef         = c't_UNDEF
  | l == PType          = c't_type
  | l == PTruth         = c't_truth_value
  | l == PPointer       = c't_pointer
  | l == PPointable     = c't_typed_pointer
  | l == PString        = c't_string
  | l == PCSChar        = c't_S8
  | l == PCUChar        = c't_U8
  | l == PCShort        = c't_S16
  | l == PCUShort       = c't_U16
  | l == PCInt          = c't_S32
  | l == PCUInt         = c't_U32
  | l == PCFloat        = c't_F32
  | l == PCDouble       = c't_F64
  | l == PTuple         = c't_tuple
  | l == PList          = c't_list
  | l == PSet           = c't_disjoint_set
  | l == PGraph         = c't_graph
  | l == PNode          = c't_node
  | l == PAttribute     = c't_attribute
  | l == PAttributeList = c't_attribute_list
  | l == PLink          = c't_link
  | l == PLinkHead      = c't_link_head
  | l == PStatistics    = c't_statistics
  | l == PRawMoments    = c't_raw_moments
  | l == PPixelImage    = c't_pixel_image

hTypeLabel :: C'type_label -> PointableType
hTypeLabel l
  | l == c't_UNDEF          = PUndef
  | l == c't_type           = PType
  | l == c't_truth_value    = PTruth
  | l == c't_pointer        = PPointer
  | l == c't_typed_pointer  = PPointable
  | l == c't_string         = PString
  | l == c't_S8             = PCSChar
  | l == c't_U8             = PCUChar
  | l == c't_S16            = PCShort
  | l == c't_U16            = PCUShort
  | l == c't_S32            = PCInt
  | l == c't_U32            = PCUInt
  | l == c't_F32            = PCFloat
  | l == c't_F64            = PCDouble
  | l == c't_tuple          = PTuple
  | l == c't_list           = PList
  | l == c't_disjoint_set   = PSet
  | l == c't_graph          = PGraph
  | l == c't_node           = PNode
  | l == c't_attribute      = PAttribute
  | l == c't_attribute_list = PAttributeList
  | l == c't_link           = PLink
  | l == c't_link_head      = PLinkHead
  | l == c't_statistics     = PStatistics
  | l == c't_raw_moments    = PRawMoments
  | l == c't_pixel_image    = PPixelImage

showPointableType :: C'type_label -> String
showPointableType l
    | l == c't_UNDEF          = "<undef>"
    | l == c't_type           = "<type>"
    | l == c't_truth_value    = "<truth_value>"
    | l == c't_pointer        = "<pointer>"
    | l == c't_typed_pointer  = "<typed_pointer>"
    | l == c't_string         = "<string>"
    | l == c't_S8             = "<S8>"
    | l == c't_U8             = "<U8>"
    | l == c't_S16            = "<S16>"
    | l == c't_U16            = "<U16>"
    | l == c't_S32            = "<S32>"
    | l == c't_U32            = "<U32>"
    | l == c't_F32            = "<F32>"
    | l == c't_F64            = "<F64>"
    | l == c't_tuple          = "<tuple>"
    | l == c't_list           = "<list>"
    | l == c't_disjoint_set   = "<disjoint_set>"
    | l == c't_graph          = "<graph>"
    | l == c't_node           = "<node>"
    | l == c't_attribute      = "<attribute>"
    | l == c't_attribute_list = "<attribute_list>"
    | l == c't_link           = "<link>"
    | l == c't_link_head      = "<link_head>"
    | l == c't_statistics     = "<statistics>"
    | l == c't_raw_moments    = "<raw_moments>"
    | l == c't_pixel_image    = "<pixel_image>"

{-
data TypedPointer =
  TypedPointer
  { tptrPtr :: !(ForeignPtr C'typed_pointer)
    tptrLabel :: TypeLabel
    tptrCount :: Int
    tptrToken :: Integer
    tptrValue :: Ptr()
  }
-}

typedPointerNull = newForeignPtr nullPtr (c'typed_pointer_free nullPtr)

typedPointerAlloc :: IO (ForeignPtr C'typed_pointer)
typedPointerAlloc = do
  ptr <- c'typed_pointer_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'typed_pointer_free ptr)
    else error "Memory allocation failed in typedPointerAlloc"

typedPointerCreate :: C'type_label -> Int -> Int -> Ptr ()
    -> IO (ForeignPtr C'typed_pointer)
typedPointerCreate l c t p = do
  ftptr <- typedPointerAlloc
  withForeignPtr ftptr $ \ptptr -> do
    r <- c'typed_pointer_create ptptr l (fromIntegral c) (fromIntegral t) p
    if r /= c'SUCCESS
      then error $ "Failed to create typed pointer with " ++ (show r)
      else return ftptr

class Pointable a where
  pointableType :: a -> PointableType
  pointableNull :: a
  pointableFromF:: ForeignPtr C'typed_pointer -> IO a
  pointableFromF ftptr =
    withForeignPtr ftptr $ \ptptr ->
      if ptptr /= nullPtr
        then do
          tptr <- peek ptptr
          pointableFrom tptr
        else return $ pointableNull
  pointableFrom :: C'typed_pointer -> IO a
  pointableInto :: a -> IO (ForeignPtr C'typed_pointer)
{-
instance Pointable () where
  pointableType _ = PUndef
  pointableNull = ()
  pointableFromF _ = return ()
  pointableFrom _ = return ()
  pointableInto _ = typedPointerNull
-}
instance Pointable (Int) where
  pointableType _ = PCInt
  pointableNull = 0
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_S8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CSChar)
    | l == c't_U8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CUChar)
    | l == c't_S16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CShort)
    | l == c't_U16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CUShort)
    | l == c't_S32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CInt)
    | l == c't_U32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CUInt)
    | otherwise    = error $ 
        "Unable to convert " ++ (showPointableType l) ++ " to Int"
  pointableInto v = do
    let
      value :: CInt
      value = fromIntegral v
    with value $ \pvalue -> typedPointerCreate c't_S32 1 0 (castPtr pvalue)

instance Pointable (Float) where
  pointableType _ = PCFloat
  pointableNull = 0
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_S8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CSChar)
    | l == c't_U8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CUChar)
    | l == c't_S16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CShort)
    | l == c't_U16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CUShort)
    | l == c't_S32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CLong)
    | l == c't_U32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CULong)
    | l == c't_F32 = liftM realToFrac   $ peek ((castPtr v)::Ptr CFloat)
    | l == c't_F64 = liftM realToFrac   $ peek ((castPtr v)::Ptr CDouble)
    | otherwise    = error $
        "Unable to convert " ++ (showPointableType l) ++ " to Float"
  pointableInto v = do
    let
      value :: CFloat
      value = realToFrac v
    with value $ \pvalue -> typedPointerCreate c't_F32 1 0 (castPtr pvalue)

instance Pointable (Double) where
  pointableType _ = PCDouble
  pointableNull = 0
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_S8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CSChar)
    | l == c't_U8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CUChar)
    | l == c't_S16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CShort)
    | l == c't_U16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CUShort)
    | l == c't_S32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CLong)
    | l == c't_U32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CULong)
    | l == c't_F32 = liftM realToFrac   $ peek ((castPtr v)::Ptr CFloat)
    | l == c't_F64 = liftM realToFrac   $ peek ((castPtr v)::Ptr CDouble)
    | otherwise    = error $
        "Unable to convert " ++ (showPointableType l) ++ " to Double"
  pointableInto v = do
    let
      value :: CDouble
      value = realToFrac v
    with value $ \pvalue -> typedPointerCreate c't_F64 1 0 (castPtr pvalue)
