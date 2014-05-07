{-#LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
            ScopedTypeVariables #-}
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

import Control.Monad
import System.IO.Unsafe

data AttribValue a => Set a =
  Set
  { setPtr :: !(ForeignPtr C'disjoint_set)
  , setId :: Int
  , setSize :: Int
  , setAttr :: Attribute a
  }

setPtrNull :: IO (ForeignPtr C'disjoint_set)
setPtrNull = newForeignPtr nullPtr (c'disjoint_set_free nullPtr)

setAlloc :: IO (ForeignPtr C'disjoint_set)
setAlloc = do
  ptr <- c'disjoint_set_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'disjoint_set_free ptr)
    else error "Memory allocation failed in setAlloc"

setCreate :: IO (Set ())
setCreate = do
  fset <- setAlloc
  withForeignPtr fset $ \pset -> do
    c'disjoint_set_create pset 0
    setFromFPtr fset AttribUnit

setNull :: IO (Set ())
setNull = do
  nptr <- setPtrNull
  return $ Set nptr 0 0 AttribUnit

setAttribUnit :: AttribValue a => Set a -> Set ()
setAttribUnit (Set p i s _) = Set p i s AttribUnit

-- | For use when must _not_ free the pointer, i.e. when creating a linking to
--   an object stored in c structures that will be destroyed by the structure
--   destructors
setFromPtr :: AttribValue a =>
    Ptr C'disjoint_set -> Attribute a -> IO (Set a)
setFromPtr pset attrib
  | pset == nullPtr = error "Null set ptr"
  | otherwise       = do
    C'disjoint_set {
      c'disjoint_set'id = p,
      c'disjoint_set'size = s
    } <- peek pset
    fp <- newForeignPtr p (c'disjoint_set_free nullPtr)
    i <- c'disjoint_set_id p
    return $ Set fp (fromIntegral i) (fromIntegral s) attrib

-- | For use when must free the pointer
setFromFPtr :: AttribValue a =>
    ForeignPtr C'disjoint_set -> Attribute a -> IO (Set a)
setFromFPtr fset attrib = withForeignPtr fset $ \pset ->
  if pset == nullPtr
   then return $ Set fset 0 0 attrib
   else do
    C'disjoint_set {
      c'disjoint_set'id = p,
      c'disjoint_set'size = s
    } <- peek pset
    i <- c'disjoint_set_id p
    return $ Set fset (fromIntegral i) (fromIntegral s) attrib

setUnion :: AttribValue a => Set a -> Set a -> IO (Set a)
setUnion s1 s2 =
  withForeignPtr (setPtr s1) $ \ps1 ->
    withForeignPtr (setPtr s2) $ \ps2 -> do
      s3 <- c'disjoint_set_union ps1 ps2
      setFromPtr s3 (setAttr s1)

setFind :: AttribValue a => Set a -> IO (Set a)
setFind s =
  withForeignPtr (setPtr s) $ \ps -> do
    ps' <- c'disjoint_set_find ps
    setFromPtr ps' (setAttr s)

setGetId :: AttribValue a => Set a -> Int
setGetId set = unsafePerformIO $
  withForeignPtr (setPtr set) $ \pset -> do
    i <- c'disjoint_set_id pset
    return $ fromIntegral i

setAttribList :: AttribValue a => Set a -> Ptr C'attribute_list
setAttribList set = unsafePerformIO $
  withForeignPtr (setPtr set) $ \pset -> return $ p'disjoint_set'attributes pset

instance Pointable (Set ()) where
  pointableType _ = PSet
  pointableNull = unsafePerformIO $ setNull
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_disjoint_set =
      setFromPtr (castPtr v) AttribUnit -- ::Ptr C'disjoint_set)
    | otherwise =
        error $ "Unable to convert " ++ (showPointableType l) ++ " to Set"
  pointableInto s = do
    fset <- setAlloc
    withForeignPtr fset $ \pset -> do
      c'disjoint_set_create pset 0
      typedPointerCreate c't_disjoint_set 1 0 (castPtr pset)

-- | AttribValue instance for Set
instance AttribValue a => AttribValue (Set a) where
  type PAttribValue (Set a) = ForeignPtr C'attribute
  type Key (Set a) = Int
  newtype (Attribute (Set a)) = PAttribSet(PAttribValue (Set a), Key (Set a), (Set a))
  attribPtr (PAttribSet(p,_,_)) = p
  attribKey (PAttribSet(_,k,_)) = k
  attribValue (PAttribSet(_,_,v)) = v
  attribIsNull (PAttribSet(_,k,_)) = k == 0
  attribInit p k v = (PAttribSet(p,k,v))
  attribCreateNull = do
    p <- attributeNull
    a <- attribCreateNull
    v <- setNull
    s <- setFromFPtr (setPtr v) a
    return (PAttribSet(p, 0, s))
  attribCreate key value
    | key == 0  = attribCreateNull
    | otherwise = do
      ftptr <- pointableInto $ setAttribUnit value
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
              v::(Set()) <- pointableFrom tptr
              s <- setFromFPtr (setPtr v) (setAttr value)
              return $ PAttribSet(fattr, (fromIntegral k), s)

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
            v::(Set()) <- pointableFrom tptr
            s <- setFromFPtr (setPtr v) (setAttr $ attribValue attrib)
            -- not my responsibility to free, but the attriblist's owner's
            fattrib <- newForeignPtr pattrib' (c'attribute_free nullPtr)
            return $ PAttribSet(fattrib, (fromIntegral k), s)
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
           v::(Set()) <- pointableFrom tptr
           s <- setFromFPtr (setPtr v) (setAttr $ attribValue attrib)
           -- not my responsibility to free, but the attriblist's owner's
           fattrib <- newForeignPtr pattrib (c'attribute_free nullPtr)
           return $ PAttribSet(fattrib, (fromIntegral k), s)
         else attribCreateNull

  attribSet attrib value pattriblist = return attrib -- TODO: implement

instance (AttribValue a) => Attributable Set a where
  type PAttributable Set a = Ptr C'disjoint_set

  createAttributed attrib pset
    | pset == nullPtr = do
      a <- attribCreateNull
      nptr <- setPtrNull
      return $ Set nptr 0 0 a
    | otherwise = do
      C'disjoint_set {
      c'disjoint_set'attributes = attriblist
      } <- peek pset
      with attriblist $ \pattriblist -> do
        nattrib <- attribGet attrib pattriblist
        setFromPtr pset nattrib

  addAttribute attrib set = attribAdd attrib (setAttribList set)

  getAttribute attrib set =
    liftM attribValue $ attribGet attrib (setAttribList set)

  setAttribute attrib value set = do
    nattrib <- attribSet attrib value (setAttribList set)
    setFromFPtr (setPtr set) nattrib

instance (AttribValue a, AttribValue b) => Extendable Set a b where
  type Target Set a b = Set (a,b)
  extendWithAttrib attrib2 (set@(Set pset key size attrib1)) = do
    attrib2' <- addAttribute attrib2 set
    return (Set pset key size
        (PAttribPair((attribPtr    attrib1, attribPtr    attrib2'),
                     (attribKey    attrib1, attribKey    attrib2'),
                     (attribValue  attrib1, attribValue  attrib2'))))
