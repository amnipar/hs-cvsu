module CVSU.Set
( Set(..)
, setCreate
, setUnion
, setFind
, setGetId
) where

import CVSU.Bindings.Set
import CVSU.Bindings.TypedPointer
import CVSU.TypedPointer

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Concurrent

import System.IO.Unsafe

data Set =
  Set
  { setPtr :: !(ForeignPtr C'disjoint_set)
  , setId :: Int
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
    c'disjoint_set_create pset
    setFromFPtr fset

-- | For use when must _not_ free the pointer, i.e. when creating a linking to
--   an object stored in c structures that will be destroyed by the structure
--   destructors
setFromPtr :: Ptr C'disjoint_set -> IO (Set)
setFromPtr pset = do
  C'disjoint_set {
    c'disjoint_set'id = p,
    c'disjoint_set'rank = r
  } <- peek pset
  fp <- newForeignPtr p (c'disjoint_set_free nullPtr)
  i <- c'disjoint_set_id p
  return $ Set fp (fromIntegral i)

-- | For use when must free the pointer
setFromFPtr :: ForeignPtr C'disjoint_set -> IO Set
setFromFPtr fset = withForeignPtr fset $ \pset -> do
  C'disjoint_set {
  c'disjoint_set'id = p,
  c'disjoint_set'rank = r
  } <- peek pset
  i <- c'disjoint_set_id p
  return $ Set fset (fromIntegral i)

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
  pointableNull = unsafePerformIO $ do
    nptr <- setPtrNull
    return $ Set nptr 0
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_disjoint_set = setFromPtr ((castPtr v)::Ptr C'disjoint_set)
    | otherwise             = error $
        "Unable to convert " ++ (showPointableType l) ++ " to Set"
  pointableInto s = do
    fset <- setAlloc
    withForeignPtr fset $ \pset -> do
      c'disjoint_set_create pset
      typedPointerCreate c't_disjoint_set 1 0 (castPtr pset)
