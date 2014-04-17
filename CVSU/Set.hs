{-#LANGUAGE TypeFamilies #-}
module CVSU.Set
( Set(..)
, setCreate
, setNull
, setUnion
, setFind
, setGetId
) where

import CVSU.Bindings.Set
import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import CVSU.Bindings.Attribute

import CVSU.TypedPointer
import CVSU.Attribute

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Concurrent

import System.IO.Unsafe

data Set =
  Set
  { setPtr :: !(ForeignPtr C'disjoint_set)
  , setId :: Int
  , setSize :: Int
  -- , setAttribute :: Attribute a
  }

setPtrNull :: IO (ForeignPtr C'disjoint_set)
setPtrNull = newForeignPtr nullPtr (c'disjoint_set_free nullPtr)

setAlloc :: IO (ForeignPtr C'disjoint_set)
setAlloc = do
  ptr <- c'disjoint_set_alloc
  if ptr /= nullPtr
     then newForeignPtr ptr (c'disjoint_set_free ptr)
     else error "Memory allocation failed in allocSet"

setCreate :: IO (Set)
setCreate = do
  fset <- setAlloc
  withForeignPtr fset $ \pset -> do
    c'disjoint_set_create pset 0
    setFromFPtr fset

setNull :: IO Set
setNull = do
  nptr <- setPtrNull
  return $ Set nptr 0 0

-- | For use when must _not_ free the pointer, i.e. when creating a linking to
--   an object stored in c structures that will be destroyed by the structure
--   destructors
setFromPtr :: Ptr C'disjoint_set -> IO (Set)
setFromPtr pset
  | pset == nullPtr = error "Null set ptr"
  | otherwise       = do
    C'disjoint_set {
      c'disjoint_set'id = p,
      c'disjoint_set'size = s
    } <- peek pset
    fp <- newForeignPtr p (c'disjoint_set_free nullPtr)
    i <- c'disjoint_set_id p
    return $ Set fp (fromIntegral i) (fromIntegral s)

-- | For use when must free the pointer
setFromFPtr :: ForeignPtr C'disjoint_set -> IO Set
setFromFPtr fset = withForeignPtr fset $ \pset -> do
  C'disjoint_set {
    c'disjoint_set'id = p,
    c'disjoint_set'size = s
  } <- peek pset
  i <- c'disjoint_set_id p
  return $ Set fset (fromIntegral i) (fromIntegral s)

setUnion :: Set -> Set -> IO (Set)
setUnion s1 s2 =
  withForeignPtr (setPtr s1) $ \ps1 ->
    withForeignPtr (setPtr s2) $ \ps2 -> do
      s3 <- c'disjoint_set_union ps1 ps2
      setFromPtr s3

setFind :: Set -> IO (Set)
setFind s =
  withForeignPtr (setPtr s) $ \ps -> do
    ps' <- c'disjoint_set_find ps
    setFromPtr ps'

setGetId :: Set -> IO Int
setGetId set =
  withForeignPtr (setPtr set) $ \pset -> do
    i <- c'disjoint_set_id pset
    return $ fromIntegral i

instance Pointable (Set) where
  pointableType _ = PSet
  pointableNull = unsafePerformIO $ setNull
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_disjoint_set = setFromPtr ((castPtr v)::Ptr C'disjoint_set)
    | otherwise             = error $
        "Unable to convert " ++ (showPointableType l) ++ " to Set"
  pointableInto s = do
    fset <- setAlloc
    withForeignPtr fset $ \pset -> do
      c'disjoint_set_create pset 0
      typedPointerCreate c't_disjoint_set 1 0 (castPtr pset)

-- | AttribValue instance for Set
instance AttribValue Set where
  type PAttribValue Set = ForeignPtr C'attribute
  type Key Set = Int
  newtype (Attribute Set) = PAttribSet(PAttribValue Set, Key Set, Set)
  attribPtr (PAttribSet(p,_,_)) = p
  attribKey (PAttribSet(_,k,_)) = k
  attribValue (PAttribSet(_,_,v)) = v
  attribIsNull (PAttribSet(_,k,_)) = k == 0
  attribInit p k v = (PAttribSet(p,k,v))
  attribCreateNull = do
    p <- attributeNull
    return (PAttribSet(p, 0, pointableNull))
  attribCreate key value
    | key == 0  = attribCreateNull
    | otherwise = do
      ftptr <- pointableInto value
      -- this will be my responsibility to free
      fattr <- attributeAlloc
      withForeignPtr fattr $ \pattr ->
        withForeignPtr ftptr $ \ptptr -> do
          r <- c'attribute_create pattr (fromIntegral key) ptptr
          if r /= c'SUCCESS
            then error $ "Failed to create attribute with " ++ (show r)
            else do
              C'attribute{
                c'attribute'key = k,
                c'attribute'value = tptr
              } <- peek pattr
              v <- pointableFrom tptr
              return $ PAttribSet(fattr, (fromIntegral k), v)

  attribAdd attrib pattriblist = do
    withForeignPtr (attribPtr attrib) $ \pattrib -> do
      let
        pattrib2 :: Ptr C'attribute
        pattrib2 = nullPtr
      with pattrib2 $ \ppattrib2 -> do
        r <- c'attribute_add pattriblist pattrib ppattrib2
        if r == c'SUCCESS
          then do
            pattrib' <- peek ppattrib2
            (C'attribute k tptr) <- peek pattrib'
            -- create needed here to set the id to correct pointer
            c'disjoint_set_create (castPtr $ c'typed_pointer'value tptr) 0
            v <- pointableFrom tptr
            -- not my responsibility to free, but the attriblist's owner's
            fattrib <- newForeignPtr pattrib' (c'attribute_free nullPtr)
            return $ PAttribSet(fattrib, (fromIntegral k), v)
          else attribCreateNull

  attribGet attrib pattriblist
    | attribKey attrib == 0  = attribCreateNull
    | otherwise = do
      pattrib <- c'attribute_find pattriblist (fromIntegral $ attribKey attrib)
      if pattrib /= nullPtr
         then do
           C'attribute{
             c'attribute'key = k,
             c'attribute'value = tptr
           } <- peek pattrib
           v <- pointableFrom tptr
           -- not my responsibility to free, but the attriblist's owner's
           fattrib <- newForeignPtr pattrib (c'attribute_free nullPtr)
           return $ PAttribSet(fattrib, (fromIntegral k), v)
         else attribCreateNull

  attribSet attrib value pattriblist = return attrib -- TODO: implement

--instance Attributable (Set) where
