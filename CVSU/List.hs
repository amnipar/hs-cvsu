module CVSU.List where

import CVSU.Bindings.List
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import System.IO.Unsafe

data ItemList a =
  ItemList {
    listPtr :: !(ForeignPtr C'list),
    hlist :: [a]
  }

allocList :: IO (Maybe (ForeignPtr C'list))
allocList = do
  ptr <- c'list_alloc
  if ptr /= nullPtr
     then do
       foreignPtr <- newForeignPtr p'list_free ptr
       return $ Just foreignPtr
     else do
       return Nothing

listFirst :: Ptr C'list -> Ptr C'list_item
listFirst l_ptr = 
  unsafePerformIO $ do
    l <- peek l_ptr
    return $ c'list_item'next $ c'list'first l

listNext :: Ptr C'list_item -> Ptr C'list_item
listNext i_ptr =
  unsafePerformIO $ do
    i <- peek i_ptr
    return $ c'list_item'next i

listLast :: Ptr C'list -> Ptr C'list_item
listLast l_ptr = p'list'last l_ptr

createList :: ForeignPtr C'list -> (Ptr C'list_item -> a) -> ItemList a
createList nullPtr _ = (ItemList nullPtr [])
createList flist op = 
  unsafePerformIO $
    withForeignPtr flist $ \l_ptr ->
      return $ ItemList flist $ recurseList (listLast l_ptr) op $ listFirst l_ptr
  where
    recurseList l f i
      | l == i    = []
      | otherwise = (f i) : (recurseList l f $ listNext i)
