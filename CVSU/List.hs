module CVSU.List
( allocList
, createList
, listFirst
, listNext
, listLast
) where

import CVSU.Bindings.List
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Concurrent

import Debug.Trace

data ItemList a =
  ItemList {
    listPtr :: !(ForeignPtr C'list),
    hlist :: [a]
  }

allocList :: IO (ForeignPtr C'list)
allocList = do
  ptr <- c'list_alloc
  if ptr /= nullPtr
     then newForeignPtr ptr (c'list_free ptr)
     else error "Memory allocation failed in allocList"

listFirst :: Ptr C'list -> IO (Ptr C'list_item)
listFirst l_ptr = do
    l <- peek l_ptr
    let pi = c'list_item'next $ c'list'first l
    i <- peek pi
    return pi

listNext :: Ptr C'list_item -> IO (Ptr C'list_item)
listNext i_ptr = do
    i <- peek i_ptr
    let pn = c'list_item'next i
    n <- peek pn
    return pn

listLast :: Ptr C'list -> IO (Ptr C'list_item)
listLast l_ptr = do
  let pl = p'list'last l_ptr
  l <- peek pl
  return pl

createList :: ForeignPtr C'list -> (Ptr C'list_item -> IO a) -> IO [a]
createList flist op =
  withForeignPtr flist $ \plist ->
    if plist == nullPtr
      then return []
      else do
        l <- listLast plist
        f <- listFirst plist
        ls <- recurseList l op f
        --ls `seq` return ls
        return $! ls
  where
    recurseList l f i
      | l == i = return []
      | otherwise = do
        n <- listNext i
        xs <- recurseList l f n
        x <- f i
        x `seq` xs `seq` return $! x:xs -- liftM (:) (f i) (recurseList l f n) ??
