module CVSU.Set
( Set(..)
, createSet
, union
, find
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

nullSetPtr = newForeignPtr nullPtr (c'disjoint_set_free nullPtr)

allocSet :: IO (ForeignPtr C'disjoint_set)
allocSet = do
  ptr <- c'disjoint_set_alloc
  if ptr /= nullPtr
     then newForeignPtr ptr (c'disjoint_set_free ptr)
     else error "Memory allocation failed in allocSet"

createSet :: IO (Set)
createSet = do
  fset <- allocSet
  withForeignPtr fset $ \pset -> do
    c'disjoint_set_create pset
    fptrToSet fset

ptrToSet :: Ptr C'disjoint_set -> IO (Set)
ptrToSet pset = do
  C'disjoint_set {
    c'disjoint_set'id = p,
    c'disjoint_set'rank = r
  } <- peek pset
  fp <- newForeignPtr p (c'disjoint_set_free p)
  i <- c'disjoint_set_id p
  return $ Set fp (fromIntegral i)

fptrToSet :: ForeignPtr C'disjoint_set -> IO Set
fptrToSet fset = withForeignPtr fset $ \pset -> ptrToSet pset

union :: Set -> Set -> IO (Set)
union s1 s2 =
  withForeignPtr (setPtr s1) $ \ps1 ->
    withForeignPtr (setPtr s2) $ \ps2 -> do
      s3 <- c'disjoint_set_union ps1 ps2
      ptrToSet s3

find :: Set -> IO (Set)
find s =
  withForeignPtr (setPtr s) $ \ps -> do
    ps' <- c'disjoint_set_find ps
    ptrToSet ps'

instance Pointable (Set) where
  pointableType _ = PSet
  pointableNull = unsafePerformIO $ do
    nptr <- nullSetPtr
    return $ Set nptr 0
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_disjoint_set = ptrToSet ((castPtr v)::Ptr C'disjoint_set)
    | otherwise             = error $
        "Unable to convert " ++ (showPointableType l) ++ " to Set"
  pointableInto s = do
    fset <- allocSet
    withForeignPtr fset $ \pset -> do
      c'disjoint_set_create pset
      typedPointerCreate c't_disjoint_set 1 0 (castPtr pset)
