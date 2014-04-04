module CVSU.Set
(

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

allocSet :: IO (ForeignPtr C'disjoint_set)
allocSet = do
  ptr <- c'disjoint_set_alloc
  if ptr /= nullPtr
     then newForeignPtr ptr (c'disjoint_set_free ptr)
     else error "Memory allocation failed in allocSet"

createSet :: Int -> IO (Set)
createSet i = do
  fset <- allocSet
  withForeignPtr fset $ \pset -> do
    c'disjoint_set_create pset $ fromIntegral i
    fptrToSet fset

ptrToSet :: Ptr C'disjoint_set -> IO Set
ptrToSet pset = do
  C'disjoint_set {
    c'disjoint_set'parent = p,
    c'disjoint_set'id = i,
    c'disjoint_set'rank = r
  } <- peek pset
  fp <- newForeignPtr p (c'disjoint_set_free p)
  return $ Set fp $ fromIntegral i

fptrToSet :: ForeignPtr C'disjoint_set -> IO Set
fptrToSet fset = withForeignPtr fset $ \pset -> ptrToSet pset
{-
instance Pointable (Set) where
  convertTo (C'typed_pointer l c t v)
    | l == c't_disjoint_set = ptrToSet ((castPtr p)::Ptr C'disjoint_set)
    | otherwise error "unable to convert to set"
    -}
--union :: Set -> Set -> Set
--union s1 s2 =
