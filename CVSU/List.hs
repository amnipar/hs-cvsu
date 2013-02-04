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
listFirst plist = do
  list <- peek plist
  return $ c'list_item'next $ c'list'first list

listNext :: Ptr C'list_item -> IO (Ptr C'list_item)
listNext pitem = do
  item <- peek pitem
  return $ c'list_item'next item

listLast :: Ptr C'list -> IO (Ptr C'list_item)
listLast plist =
  return $ p'list'last plist

createList :: ForeignPtr C'list -> (Ptr C'list_item -> IO a) -> IO [a]
createList flist op =
  withForeignPtr flist $ \plist ->
    if plist == nullPtr
      then return []
      else do
        l <- listLast plist
        f <- listFirst plist
        ls <- recurseList op l f
        --ls `seq` return ls
        return $! ls
  where
    recurseList op l i
      | l == i = return []
      | otherwise = do
        n <- listNext i
        xs <- recurseList op l n
        x <- op i
        x `seq` xs `seq` return $! x:xs -- liftM (:) (f i) (recurseList l f n) ??
