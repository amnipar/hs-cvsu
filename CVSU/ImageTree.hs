{-# LANGUAGE OverlappingInstances #-}

module CVSU.ImageTree
( ImageBlock(..)
, ImageTree(..)
, NeighborImageTree(..)
, ImageForest(..)
, NeighborImageForest(..)
, ForestValue(..)
, withForest
, mapDeep
, filterForest
, treeNeighbors
, treeClassFind
, treeClassUnion
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.List
import CVSU.Bindings.OpenCV
import CVSU.Bindings.ImageTree
import CVSU.Types
import CVSU.PixelImage
import CVSU.List

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Exception hiding (block)
import Control.Parallel.Strategies
import Control.DeepSeq
import CV.Image
import GHC.Float
import Debug.Trace

data ImageBlock v =
  ImageBlock{
    n :: Float,
    e :: Float,
    s :: Float,
    w :: Float,
    value :: !v
    } deriving Eq

instance (Show v) => Show (ImageBlock v) where
  show (ImageBlock bn be bs bw bv) =
    "(B "++(show bn)++","++(show be)++","++(show bs)++","++(show bw)++","++(show bv)++")"

data ImageTree v = EmptyTree |
  ImageTree
  { treePtr :: Ptr C'image_tree
  , classId :: Integer
  , block :: !(ImageBlock v)
  , nw :: ImageTree v
  , ne :: ImageTree v
  , sw :: ImageTree v
  , se :: ImageTree v
  } deriving Eq

instance (Show a) => Show (ImageTree a) where
  show EmptyTree = "(B)"
  show ImageTree{ block = b } = show b

newtype NeighborImageTree a = NIT(ImageTree a)

data ImageForest v =
  ImageForest
  { forestPtr :: !(ForeignPtr C'image_tree_forest)
  , img :: PixelImage
  , rows :: Int
  , cols :: Int
  , blockType :: ImageBlockType
  , trees :: ![ImageTree v]
  }

instance (Show a) => Show (ImageForest a) where
  show (ImageForest p i r c t ts) = "(F " ++
    (show r) ++ "x" ++ (show c) ++ " " ++ (show ts) ++ ")"

newtype NeighborImageForest a = NIF(ImageForest a)

instance Eq (ImageForest a) where
  (==) f1 f2 = (forestPtr f1) == (forestPtr f2)

-- make ImageBlock a functor and applicative to allow fmapping

instance Functor ImageBlock where
  fmap f (ImageBlock bn be bs bw bv) = (ImageBlock bn be bs bw (f bv))

instance Applicative ImageBlock where
  pure v = (ImageBlock 0 0 0 0 v)
  ImageBlock{ value = f } <*> b = fmap f b

instance Functor ImageTree where
  fmap f EmptyTree = EmptyTree
  fmap f (ImageTree ptr cid b tnw tne tsw tse) =
    (ImageTree ptr cid (fmap f b) (fmap f tnw) (fmap f tne) (fmap f tsw) (fmap f tse))

instance Applicative ImageTree where
  pure v = (ImageTree nullPtr 0 (pure v) EmptyTree EmptyTree EmptyTree EmptyTree)
  ImageTree{ block = ImageBlock{ value = f } } <*> b = fmap f b

instance Functor ImageForest where
  fmap f (ImageForest ptr i r c t ts) = (ImageForest ptr i r c t (mapDeep (fmap f) ts))

class ForestValue a where
  checkType :: ImageForest a -> Bool
  toValue :: Ptr() -> IO (a)
  getTree :: ImageForest a -> (Int,Int) -> IO (ImageTree a)
  getTree f (x,y) =
    withForeignPtr (forestPtr f) $ \pforest -> do
      forest <- peek pforest
      root <- peek $ advancePtr (c'image_tree_forest'roots forest) (y * (cols f) + x)
      (treeFromPtr toValue) $ treePtrFromRoot root
  initForest :: PixelImage -> (Int,Int) -> IO (ImageForest a)
  createForest :: PixelImage -> (Int,Int) -> IO (ImageForest a)
  createForest i (w,h) = do
    s <- initForest i (w,h)
    if not $ checkType s
       then error "forest content type is invalid"
       else updateForest s
  updateForest :: ImageForest a -> IO (ImageForest a)
  updateForest (ImageForest pforeign i r c t _) = do
    withForeignPtr pforeign $ \pforest -> do
      result <- c'image_tree_forest_update pforest
      if result /= c'SUCCESS
         then error "forest update failed"
         else do
           C'image_tree_forest{
             c'image_tree_forest'roots = proots
           } <- peek pforest
           rs <- peekArray (r*c) proots
           ts <- mapM (treeFromPtr toValue) $ map treePtrFromRoot rs
           return $ ImageForest pforeign i r c t ts

instance ForestValue Stat where
  checkType = (==BlockStatGrey).blockType
  initForest = createForestStub BlockStatGrey
  toValue p = do
    C'stat_grey{
      c'stat_grey'mean = m,
      c'stat_grey'dev = d
    } <- peek ((castPtr p)::Ptr C'stat_grey)
    return $ Stat(fromIntegral m, fromIntegral d)

instance ForestValue StatColor where
  checkType = (==BlockStatColor).blockType
  initForest = createForestStub BlockStatColor
  toValue p = do
    C'stat_color{
      c'stat_color'mean_i = m,
      c'stat_color'dev_i = d,
      c'stat_color'mean_c1 = m1,
      c'stat_color'dev_c1 = d1,
      c'stat_color'mean_c2 = m2,
      c'stat_color'dev_c2 = d2
    } <- peek ((castPtr p)::Ptr C'stat_color)
    return $ StatColor(
      Stat((fromIntegral m),(fromIntegral d)),
      Stat((fromIntegral m1),(fromIntegral d1)),
      Stat((fromIntegral m2),(fromIntegral d2)))

createForestStub :: (ForestValue v) => ImageBlockType -> PixelImage -> (Int,Int) -> IO (ImageForest v)
createForestStub t i (w,h) = do
  mforeign <- allocImageTreeForest
  if isNothing mforeign
    then error "failed to allocate forest pointer"
    else do
      let pforeign = fromJust mforeign
      withForeignPtr pforeign $ \pforest ->
        withForeignPtr (imagePtr i) $ \pimage -> do
          result <- c'image_tree_forest_create pforest pimage 
            (fromIntegral w) (fromIntegral h) (cImageBlockType t)
          if result /= c'SUCCESS
             then error "failed to create forest"
             else do
               C'image_tree_forest{
                 c'image_tree_forest'rows = r,
                 c'image_tree_forest'cols = c,
                 c'image_tree_forest'type = t
               } <- peek pforest
               return $ ImageForest pforeign i (fromIntegral r) (fromIntegral c) (hImageBlockType t) []

treePtrFromRoot :: C'image_tree_root -> Ptr C'image_tree
treePtrFromRoot r = c'image_tree_root'tree r

treeFromPtr :: (ForestValue v) => (Ptr() -> IO v) -> Ptr C'image_tree -> IO (ImageTree v)
treeFromPtr conv ptree
  | ptree == nullPtr = return EmptyTree
  | otherwise      = do
    tree <- peek ptree
    cid <- c'image_tree_class_get ptree
    (C'image_block x y w h pvalue) <- peek (c'image_tree'block tree)
    v <- conv pvalue
    tnw <- treeFromPtr conv (c'image_tree'nw tree)
    tne <- treeFromPtr conv (c'image_tree'ne tree)
    tsw <- treeFromPtr conv (c'image_tree'sw tree)
    tse <- treeFromPtr conv (c'image_tree'se tree)
    let
      b = ImageBlock bn be bs bw v
      bn = fromIntegral y
      be = fromIntegral x + fromIntegral w
      bs = fromIntegral y + fromIntegral h
      bw = fromIntegral x
    return $ ImageTree ptree (fromIntegral cid) b tnw tne tsw tse

-- allocate image_tree_forest structure using c function and foreign pointers
-- image_tree_forest_alloc is used for allocating the image struct
-- image_tree_forest_free is specified as finalizer
allocImageTreeForest :: IO (Maybe (ForeignPtr C'image_tree_forest))
allocImageTreeForest = do
  ptr <- c'image_tree_forest_alloc
  if ptr /= nullPtr
    then do
      foreignPtr <- newForeignPtr p'image_tree_forest_free ptr
      return $ Just foreignPtr
    else do
      return Nothing

withForest :: ImageForest a -> (ImageForest a -> IO b) -> IO b
withForest f op = -- @ImageForest{ forestPtr = ptr }
  withForeignPtr (forestPtr f) $ \_ -> do
    r <- op f
    touchForeignPtr (forestPtr f)
    return $! r

deep :: NFData a => a -> a
deep a = deepseq a a

instance NFData (ImageTree a) where
  rnf (ImageTree _ _ b tnw tne tsw tse) = tnw `seq` tne `seq` tsw `seq` tse `seq` b `seq` ()

instance NFData (ImageBlock a) where
  rnf (ImageBlock bn be bs bw bv) = bn `seq` be `seq` bs `seq` bw `seq` bv `seq` ()

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
  b <- rpar (f a)
  bs <- parMap' f as
  return (b:bs)

mapDeep :: (NFData a, NFData b) => (a -> b) -> [a] -> [b]
mapDeep _ [] = []
mapDeep op xs =
  unsafePerformIO $ do
    evaluate $! deep $ runEval $ parMap' op $ xs

filterForest :: (a -> Bool) -> ImageForest a -> ImageForest a
filterForest cond (ImageForest ptr i r c t ts) =
  (ImageForest ptr i r c t (filter (cond . value . block) $! ts))

-- | Creates an image tree from a list item by casting and converting
treeFromListItem :: (ForestValue a) => Ptr C'list_item -> IO (ImageTree a)
treeFromListItem pitem = do
  item <- peek pitem
  ptree <- peek $ castPtr $ c'list_item'data item
  treeFromPtr toValue $ castPtr $ ptree

treeNeighbors :: (ForestValue b) => ImageTree a -> IO [ImageTree b]
treeNeighbors t = do
  flist <- allocList
  withForeignPtr flist $ \plist -> do
    r1 <- c'image_tree_create_neighbor_list plist
    if r1 /= c'SUCCESS
      then error "Failed to create tree neighbor list"
      else do
        r2 <- c'image_tree_find_all_immediate_neighbors plist (treePtr t)
        if r2 /= c'SUCCESS
          then error "Finding tree neighbors failed"
          else do
            ls <- createList flist treeFromListItem
            return $! ls

treeClassInit :: ImageTree a -> IO (ImageTree a)
treeClassInit t@(ImageTree ptr _ b nw ne sw se) = do
  c'image_tree_class_create ptr
  cid <- treeClassFind t
  return $ ImageTree ptr cid b nw ne sw se

treeClassUnion :: (ImageTree a, ImageTree a) -> IO (ImageTree a, ImageTree a)
treeClassUnion (t1@(ImageTree ptr1 _ b1 nw1 ne1 sw1 se1),
                t2@(ImageTree ptr2 _ b2 nw2 ne2 sw2 se2)) = do
  c'image_tree_class_create ptr1
  c'image_tree_class_create ptr2
  c'image_tree_class_union ptr1 ptr2
  cid1 <- treeClassFind t1
  cid2 <- treeClassFind t2
  return $! ((ImageTree ptr1 cid1 b1 nw1 ne1 sw1 se1),
          (ImageTree ptr2 cid2 b2 nw2 ne2 sw2 se2))

treeClassFind :: ImageTree a -> IO (Integer)
treeClassFind t = do
  pid <- c'image_tree_class_find (treePtr t)
  tid <- c'image_tree_class_get (treePtr t)
  return $ fromIntegral tid
