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
  LabelStatistics

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

data TypedPointer =
  TypedPointer
  { tptrPtr :: !(ForeignPtr C'typed_pointer)
    tptrLabel :: TypeLabel
    tptrCount :: Int
    tptrToken :: Integer
    tptrValue :: Ptr()
  }

typedPointerAlloc :: IO (ForeignPtr C'typed_pointer)

typedPointerInit :: Ptr C'typed_pointer -> C'type_label -> Int -> IO ()
typedPointerInit tptr l c = do
  r <- c'typed_pointer'create tptr l (fromIntegral c)
  if r /= c'SUCCESS
    then error "Failed to initialize typed_pointer"
    else return

class Pointable a where
  tptrTo :: C'typed_pointer -> IO a
  tptrFrom :: a -> IO (ForeignPtr C'typed_pointer)

instance Pointable (Int) where
  tptrTo :: C'typed_pointer -> Int
  tptrTo (C'typed_pointer l c t v)
    | l == c't_S8  = liftM fromIntegral $ peek ((castPtr p)::Ptr CSChar)
    | l == c't_U8  = liftM fromIntegral $ peek ((castPtr p)::Ptr CUChar)
    | l == c't_S16 = liftM fromIntegral $ peek ((castPtr p)::Ptr CSShort)
    | l == c't_U16 = liftM fromIntegral $ peek ((castPtr p)::Ptr CUShort)
    | l == c't_S32 = liftM fromIntegral $ peek ((castPtr p)::Ptr CLong)
    | l == c't_U32 = liftM fromIntegral $ peek ((castPtr p)::Ptr CULong)
    | otherwise error "unable to convert " ++ (show l) ++ " to Int"
  tptrFrom :: Integral i => i -> IO (ForeignPtr C'typed_pointer)
  tptrFrom i = do
    ftptr <- typedPointerAlloc
    withForeignPtr ftptr $ \tptr -> do
      typedPointerInit tptr c't_S32 1
      let
        value :: CLong
        value = fromIntegral i
      with value $ \pvalue -> do
        r <- c'typed_pointer_set_value tptr 0 (castPtr pvalue)
        if r /= c'SUCCESS
          then error "Failed to set value of typed_pointer"
          else return ftptr

--instance Pointable (Float) where