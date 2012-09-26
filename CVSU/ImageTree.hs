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
  fmap f (ImageTree ptr b tnw tne tsw tse) =
    (ImageTree ptr (fmap f b) (fmap f tnw) (fmap f tne) (fmap f tsw) (fmap f tse))

instance Applicative ImageTree where
  pure v = (ImageTree nullPtr (pure v) EmptyTree EmptyTree EmptyTree EmptyTree)
  ImageTree{ block = ImageBlock{ value = f } } <*> b = fmap f b

instance Functor ImageForest where
  fmap f (ImageForest ptr i r c t ts) = (ImageForest ptr i r c t (mapDeep (fmap f) ts))

class ForestValue a where
  checkType :: ImageForest a -> Bool
  toValue :: Ptr() -> IO (a)
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
treeFromPtr conv ptr
  | ptr == nullPtr = return EmptyTree
  | otherwise      = do
    t <- peek ptr
    (C'image_block x y w h pvalue) <- peek (c'image_tree'block t)
    v <- conv pvalue
    tnw <- treeFromPtr conv (c'image_tree'nw t)
    tne <- treeFromPtr conv (c'image_tree'ne t)
    tsw <- treeFromPtr conv (c'image_tree'sw t)
    tse <- treeFromPtr conv (c'image_tree'se t)
    let
      b = ImageBlock bn be bs bw v
      bn = fromIntegral y
      be = fromIntegral x + fromIntegral w
      bs = fromIntegral y + fromIntegral h
      bw = fromIntegral x
    return $ ImageTree ptr b tnw tne tsw tse

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

withForest :: ImageForest a -> (ImageForest a -> b) -> IO b
withForest f@ImageForest{ forestPtr = ptr } op =
  withForeignPtr (forestPtr f) $ \_ -> do
    r <- do return $! op f
    touchForeignPtr $! ptr
    return r

deep :: NFData a => a -> a
deep a = deepseq a a

instance NFData (ImageTree a) where
  rnf (ImageTree _ b tnw tne tsw tse) = tnw `seq` tne `seq` tsw `seq` tse `seq` b `seq` ()

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
