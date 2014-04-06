module CVSU.Set
( Set(..)
, createSet
, union
, find
) where

import CVSU.Bindings.Set

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Concurrent

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
    c'disjoint_set'parent = p,
    c'disjoint_set'rank = r
  } <- peek pset
  fp <- newForeignPtr p (c'disjoint_set_free p)
  i <- c'disjoint_set_id p
  return $ Set fp (fromIntegral i)

fptrToSet :: ForeignPtr C'disjoint_set -> IO Set
fptrToSet fset = withForeignPtr fset $ \pset -> ptrToSet pset

union :: Set -> Set -> IO (Set)
union s1 s2 = do
  withForeignPtr (setPtr s1) $ \ps1 ->
    withForeignPtr (setPtr s2) $ \ps2 ->
      s3 <- c'disjoint_set_union ps1 ps2
      ptrToSet s3

find :: Set -> IO (Set)
find s = do
  withForeignPtr (setPtr s) $ \ps ->
    ps' <- c'disjoint_set_find ps
    ptrToSet s'

instance Pointable (Set) where
  fromTypedPointer (C'typed_pointer l c t v)
    | l == c't_disjoint_set = ptrToSet ((castPtr v)::Ptr C'disjoint_set)
    | otherwise error "unable to convert " ++ (showTypeLabel l) ++ " to Set"
  intoFTypedPointer s = do
    fset <- allocSet
    withForeignPtr fset $ \pset -> do
      c'disjoint_set_create pset
      return fptr
  nullPointable = do
    nptr <- nullSetPtr
    return $ Set nptr 0
