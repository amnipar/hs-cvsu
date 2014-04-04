module CVSU.TypedPointer
(

) where

import CVSU.Bindings.TypedPointer
import Foreign.C.Types
import Foreign.Ptr

data TypeLabel =
  LabelUndef |
  LabelType |
  LabelTruthValue |
  LabelPointer |
  LabelTypedPointer |
  LabelString |
  LabelCS8 |
  LabelCU8 |
  LabelCS16 |
  LabelCU16 |
  LabelCS32 |
  LabelCU32 |
  LabelCF32 |
  LabelCF64 |
  LabelTuple |
  LabelList |
  LabelSet |
  LabelGraph |
  LabelNode |
  LabelAttribute |
  LabelAttributeList |
  LabelLink |
  LabelLinkHead |
  LabelStatistics |
  LabelPixelImage

cTypeLabel :: TypeLabel -> C'type_label
cTypeLabel l
  | l == LabelUndef = c't_UNDEF
  | l == LabelType = c't_type
  | l == LabelTruthValue = c't_truth_value
  | l == LabelPointer = c't_pointer
  | l == LabelTypedPointer = c't_typed_pointer
  | l == LabelString = c't_string
  | l == LabelCS8 = c't_S8
  | l == LabelCU8 = c't_U8
  | l == LabelCS16 = c't_S16
  | l == LabelCU16 = c't_U16
  | l == LabelCS32 = c't_S32
  | l == LabelCU32 = c't_U32
  | l == LabelCF32 = c't_F32
  | l == LabelCF64 = c't_F64
  | l == LabelTuple = c't_tuple
  | l == LabelList = c't_list
  | l == LabelSet = c't_set
  | l == LabelGraph = c't_graph
  | l == LabelNode = c't_node
  | l == LabelAttribute = c't_attribute
  | l == LabelAttributeList = c't_attribute_list
  | l == LabelLink = c't_link
  | l == LabelLinkHead = c't_link_head
  | l == LabelStatistics = c't_statistics
  | l == LabelPixelImage = c't_pixel_image

hTypeLabel :: C'type_label -> TypeLabel
hTypeLabel l
  | l == c't_UNDEF = LabelUndef
  | l == c't_type = LabelType
  | l == c't_truth_value = LabelTruthValue
  | l == c't_pointer = LabelPointer
  | l == c't_typed_pointer = LabelTypedPointer
  | l == c't_string = LabelString
  | l == c't_S8 = LabelCS8
  | l == c't_U8 = LabelCU8
  | l == c't_S16 = LabelCS16
  | l == c't_U16 = LabelCU16
  | l == c't_S32 = LabelCS32
  | l == c't_U32 = LabelCU32
  | l == c't_F32 = LabelCF32
  | l == c't_F64 = LabelCF64
  | l == c't_tuple = LabelTuple
  | l == c't_list = LabelList
  | l == c't_set = LabelSet
  | l == c't_graph = LabelGraph
  | l == c't_node = LabelNode
  | l == c't_attribute = LabelAttribute
  | l == c't_attribute_list = LabelAttributeList
  | l == c't_link = LabelLink
  | l == c't_link_head = LabelLinkHead
  | l == c't_statistics = LabelStatistics
  | l == c't_pixel_image = LabelPixelImage

instance Show (C'type_label) where
  show l
    | l == c't_UNDEF =          "<undef>"
    | l == c't_type  =          "<type>"
    | l == c't_truth_value =    "<truth_value>"
    | l == c't_pointer =        "<pointer>"
    | l == c't_typed_pointer =  "<typed_pointer>"
    | l == c't_string =         "<string>"
    | l == c't_S8 =             "<S8>"
    | l == c't_U8 =             "<U8>"
    | l == c't_S16 =            "<S16>"
    | l == c't_U16 =            "<U16>"
    | l == c't_S32 =            "<S32>"
    | l == c't_U32 =            "<U32>"
    | l == c't_F32 =            "<F32>"
    | l == c't_F64 =            "<F64>"
    | l == c't_tuple =          "<tuple>"
    | l == c't_list =           "<list>"
    | l == c't_set =            "<set>"
    | l == c't_graph =          "<graph>"
    | l == c't_node =           "<node>"
    | l == c't_attribute =      "<attribute>"
    | l == c't_attribute_list = "<attribute_list>"
    | l == c't_link =           "<link>"
    | l == c't_link_head =      "<link_head>"
    | l == c't_statistics =     "<statistics>"
    | l == c't_pixel_image =    "<pixel_image>"

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

typedPointerAlloc :: IO (ForeignPtr C'typed_pointer)
typedPointerAlloc = do
  ptr <- c'typed_pointer'alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'typed_pointer_free ptr)
    else error "Memory allocation failed in typedPointerAlloc"

typedPointerCreate :: C'type_label -> Int -> Ptr ()
    -> IO (ForeignPtr C'typed_pointer)
typedPointerCreate l c p = do
  ftptr <- typedPointerAlloc
  withForeignPtr ftptr $ \ptptr -> do
    r <- c'typed_pointer_create ptptr l (fromIntegral c) p
    if r /= c'SUCCESS
      then error "Failed to create typed pointer with " ++ (show r)
      else return

class Pointable a where
  fromTypedPointer :: ForeignPtr C'typed_pointer -> IO a
  fromTypedPointer ftptr =
    withForeignPtr ftptr $ \ptptr -> convertFrom (peek ptptr)
  convertFrom :: C'typed_pointer -> IO a
  intoTypedPointer :: a -> IO (ForeignPtr C'typed_pointer)

instance Pointable (Int) where
  convertFrom :: C'typed_pointer -> IO Int
  convertFrom (C'typed_pointer l c t v)
    | l == c't_S8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CSChar)
    | l == c't_U8  = liftM fromIntegral $ peek ((castPtr v)::Ptr CUChar)
    | l == c't_S16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CSShort)
    | l == c't_U16 = liftM fromIntegral $ peek ((castPtr v)::Ptr CUShort)
    | l == c't_S32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CLong)
    | l == c't_U32 = liftM fromIntegral $ peek ((castPtr v)::Ptr CULong)
    | otherwise error "unable to convert " ++ (show l) ++ " to Int"
  intoTypedPointer :: Integral i => i -> IO (ForeignPtr C'typed_pointer)
  intoTypedPointer i = do
      let
        value :: CLong
        value = fromIntegral i
      with value $ \pvalue -> typedPointerCreate c't_S32 1 (castPtr pvalue)

--instance Pointable (Float) where