{-# LANGUAGE OverlappingInstances #-}

module CVSU.ImageTree
( ImageBlock(..)
, ImageTree(..)
, NeighborImageTree(..)
, ImageForest(..)
, NeighborImageForest(..)
, readImageForest
, readCVImageForest
, reloadForest
, loadForest
, withForest
, withForestFromImage
, withForestFromGreyCVImage
, withForestFromColorCVImage
, forestImage
, forestSize
, forestGetSize
, touchForest
, updateForest
, mapDeep
, updateTree
, divideForest
, divideWithDevBigger
, divideTree
, findTreeNeighbors
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
  NullForest |
  ImageForest
  { forestPtr :: !(ForeignPtr C'image_tree_forest)
  , img :: PixelImage
  , rows :: Int
  , cols :: Int
  , blockType :: ImageBlockType
  , trees :: ![ImageTree v]
  }

instance (Show a) => Show (ImageForest a) where
  show NullForest = "(F)"
  show (ImageForest p i r c ts) = "(F " ++
    (show r) ++ "x" ++ (show c) ++ " " ++ (show ts) ++ ")"

newtype NeighborImageForest a = NIF(ImageForest a)

instance Eq (ImageForest a) where
  (==) NullForest NullForest = True
  (==) (ImageForest ap _ _ _ _) (ImageForest bp _ _ _ _) = ap == bp
  (==) _ _ = False

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
  fmap f NullForest = NullForest
  fmap f (ImageForest ptr i r c ts) = (ImageForest ptr i r c (mapDeep (fmap f) ts))

treeFromPtr :: (ForestValue v) => Ptr C'image_tree -> ImageTree v
treeFromPtr _ nullPtr = EmptyTree
treeFromPtr conv ptr = unsafePerformIO $ do
    t <- peek ptr
    C'image_block{
    c'image_block'x = x,
    c'image_block'y = y,
    c'image_block'w = w,
    c'image_block'h = h,
    c'image_block'value = vptr
    } <- peek (c'image_tree'block t)
    v <- fromPtr v
    return $
      ImageTree
      { treePtr = ptr
      , block =
        ImageBlock
        { n = (fromIntegral y)
        , e = (fromIntegral x) + (fromIntegral w)
        , s = (fromIntegral y) + (fromIntegral h)
        , w = (fromIntegral x)
        , value = fromPtr v
        }
      , nw = treeFromPtr $ c'image_tree'nw t
      , ne = treeFromPtr $ c'image_tree'ne t
      , sw = treeFromPtr $ c'image_tree'sw t
      , se = treeFromPtr $ c'image_tree'se t
      }

rootToImageTree :: (ForestValue v) => C'image_tree_root -> ImageTree v
rootToImageTree r = treeFromPtr (c'image_tree_root'tree r)

forestFromPtr :: (ForestValue v) => ForeignPtr C'image_tree_forest -> IO (ImageForest v)
forestFromPtr ptr = do
  withForeignPtr ptr $ \fPtr -> do
    C'image_tree_forest{
      c'image_tree_forest'original = i_ptr,
      c'image_tree_forest'rows = r,
      c'image_tree_forest'cols = c,
      c'image_tree_forest'type = t,
      c'image_tree_forest'roots = rPtr
    } <- peek fPtr
    --i <- ptrToPixelImage True i_ptr
    rs <- peekArray ((fromIntegral r) * (fromIntegral c)) rPtr
    return $
      ImageForest
      { forestPtr = ptr
      , img = NullImage
      , rows = (fromIntegral r)
      , cols = (fromIntegral c)
      , blockType = (hImageBlockType t)
      , trees = map rootToImageTree rs
      }

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

withForestFromImage :: (ForestValue v) => String -> Int -> Int -> (ImageForest v -> b) -> IO b
withForestFromImage s w h op = do
  ptr <- allocImageTreeForest
  if isNothing ptr
    then do
      print $ "Failed to allocate forest for file " ++ s
      return $ op NullForest
    else do
      withCString s $ \name -> do
        withForeignPtr (fromJust ptr) $ \f_ptr -> do
          result <- c'image_tree_forest_read f_ptr name (fromIntegral w) (fromIntegral h)
          if result == c'SUCCESS
            then do
              f <- forestFromPtr (fromJust ptr)
              r <- do return $! op f
              --print $ "after op with file " ++ s
              touchForeignPtr $! fromJust ptr --forestPtr f
              --print $ "after touch with file " ++ s
              return r
            else do
              print $ "Failed to read forest from file " ++ s ++ ", result " ++ (show result)
              return $ op NullForest



withForestFromGreyCVImage :: (ForestValue v) => Image GrayScale D8 -> Int -> Int
  -> (ImageForest v -> b) -> IO b
withForestFromGreyCVImage i w h op = do
  withGenImage i $ \image -> do
    pimg <- allocPixelImage
    imgf <- allocImageTreeForest
    if isNothing pimg || isNothing imgf
      then do
        print $ "Failed to allocate structures"
        return $ op NullForest
      else do
        withForeignPtr (fromJust pimg) $ \i_ptr -> do
          presult <- c'pixel_image_create_from_ipl_image i_ptr image c'GREY
          if presult == c'SUCCESS
            then do
              withForeignPtr (fromJust imgf) $ \f_ptr -> do
                fresult <- c'image_tree_forest_create f_ptr i_ptr
                  (fromIntegral w) (fromIntegral h) c'b_STAT_GREY
                if fresult == c'SUCCESS
                  then do
                    f <- forestFromPtr $ fromJust imgf
                    r <- do return $! op f
                    --print $ "after op with file " ++ s
                    touchForeignPtr $! fromJust imgf --forestPtr f
                    --print $ "after touch with file " ++ s
                    return r
                  else do
                    print $ "Failed to create image forest"
                    return $ op NullForest
            else do
              print $ "Failed to create pixel image"
              return $ op NullForest

withForestFromColorCVImage :: (Image RGB D8) -> Int -> Int -> (ImageForest StatColor -> b) -> IO b
withForestFromColorCVImage i w h op = do
  withGenImage i $ \image -> do
    pimg <- allocPixelImage
    imgf <- allocImageTreeForest
    if isNothing pimg || isNothing imgf
      then do
        print $ "Failed to allocate structures"
        return $ op NullForest
      else do
        withForeignPtr (fromJust pimg) $ \i_ptr -> do
          presult <- c'pixel_image_create_from_ipl_image i_ptr image c'RGB
          if presult == c'SUCCESS
            then do
              withForeignPtr (fromJust imgf) $ \f_ptr -> do
                fresult <- c'image_tree_forest_create f_ptr i_ptr
                (fromIntegral w) (fromIntegral h) c'b_STAT_GREY
                if fresult == c'SUCCESS
                  then do
                    f <- forestFromPtr $ fromJust imgf
                    r <- do return $! op f
                    --print $ "after op with file "
                    touchForeignPtr $! fromJust imgf --forestPtr f
                    --print $ "after touch with file "
                    return r
                  else do
                    print $ "Failed to create image forest"
                    return $ op NullForest
            else do
              print $ "Failed to create pixel image"
              return $ op NullForest

readImageForest :: (ForestValue v) => String -> Int -> Int -> IO (Maybe (ImageForest v))
readImageForest str w h = do --unsafePerformIO $ do
  -- allocate the struct - deallocated automatically by garbage collector
  ptr <- allocImageTreeForest
  if isNothing ptr
    then do
      return $ Nothing
    else do
      withCString str $ \name -> do
        withForeignPtr (fromJust ptr) $ \f_ptr -> do
          result <- c'image_tree_forest_read f_ptr name (fromIntegral w) (fromIntegral h)
          if result == c'SUCCESS
            then do
              f <- forestFromPtr (fromJust ptr)
              return $ Just f
            else do
              return $ Nothing

readCVImageForest :: (ForestValue v) => Image RGB D8 -> Int -> Int -> IO (ImageForest v)
readCVImageForest i w h =
  withGenImage i $ \image -> do
    pimg <- allocPixelImage
    imgf <- allocImageTreeForest
    if isNothing pimg || isNothing imgf
      then do
        print $ "Error: failed to allocate structures"
        return $ NullForest
      else do
        withForeignPtr (fromJust pimg) $ \i_ptr -> do
          presult <- c'pixel_image_create_from_ipl_image i_ptr image c'RGB
          if presult == c'SUCCESS
            then do
              withForeignPtr (fromJust imgf) $ \f_ptr -> do
                fresult <- c'image_tree_forest_create f_ptr i_ptr
                (fromIntegral w) (fromIntegral h) c'b_STAT_GREY
                if fresult == c'SUCCESS
                  then do
                    forestFromPtr $ fromJust imgf
                  else do
                    print $ "Error: failed to create image forest"
                    return $ NullForest
            else do
              print $ "Error: failed to create pixel image from CV image"
              return $ NullForest

reloadForest :: (ForestValue v) => ImageForest a -> Int -> Int -> ImageForest v
reloadForest f w h =
  unsafePerformIO $
    withForeignPtr (forestPtr f) $ \f_ptr -> do
      result <- c'image_tree_forest_reload f_ptr (fromIntegral w) (fromIntegral h) c'b_STAT_GREY
      if result == c'SUCCESS
        then
          forestFromPtr $ forestPtr f
        else
          return $ NullForest

loadForest :: FilePath -> Int -> Int -> ImageForest StatColor
loadForest p w h =
  unsafePerformIO $ do
    image <- readFromFile p
    forest <- readCVImageForest image w h
    return forest

-- TODO: should perhaps make a clone of the image instead of repossessing it
-- this will cause problems if same forest structure is used with multiple images
-- (like an image sequence or a video stream)
forestImage :: (ImageForest a) -> IO (PixelImage)
forestImage f =
  withForeignPtr (forestPtr f) $ \f_ptr -> do
    C'image_tree_forest{
      c'image_tree_forest'original = i_ptr
    } <- peek f_ptr
    return NullImage -- ptrToPixelImage True i_ptr

--forestWithImage :: (ImageForest a) -> (ImageForest a)
--forestWithImage f@(ImageForest ptr i r c ts)
--  | i /= NullImage = f
--  | otherwise = (ImageForest ptr (lift $ forestImage f) r c ts)

forestSize :: (ImageForest a) -> IO (Int, Int)
forestSize NullForest = return (0,0)
forestSize f =
  withForeignPtr (forestPtr f) $ \f_ptr -> do
    i_ptr <- peek $ p'image_tree_forest'original f_ptr
    w <- peek $ p'pixel_image'width i_ptr
    h <- peek $ p'pixel_image'height i_ptr
    return (fromIntegral w, fromIntegral h)

forestGetSize :: (ImageForest a) -> (Int, Int)
forestGetSize f =
  unsafePerformIO $ do
    (w,h) <- forestSize f
    return (w,h)

touchForest :: ImageForest a -> ImageForest a
touchForest f@ImageForest{ forestPtr = ptr } =
  unsafePerformIO $ do
    touchForeignPtr ptr
    print $ "touch"
    return f

withForest :: ImageForest a -> (ImageForest a -> b) -> IO b
withForest NullForest op = return $ op NullForest
withForest f@ImageForest{ forestPtr = ptr } op =
  withForeignPtr ptr $ \f_ptr -> do
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

updateForest :: ImageForest StatColor -> ImageForest StatColor
updateForest NullForest = NullForest
updateForest f@(ImageForest ptr i r c ts) =
  unsafePerformIO $ do
    --print $ "updateForest"
    withForeignPtr ptr $ \f_ptr -> do
      result <- c'image_tree_forest_update_prepare f_ptr
      if result == c'SUCCESS
        then do
          return $ (ImageForest ptr i r c (mapDeep updateTree ts))
        else do
          print $ "Error: " ++ (show result)
          return f

updateTree :: ImageTree StatColor -> ImageTree StatColor
updateTree EmptyTree = EmptyTree
updateTree t@ImageTree{ treePtr = ptr } =
  unsafePerformIO $ do
    C'image_tree{ c'image_tree'root = r } <- peek ptr
    result <- c'image_tree_root_update r
    if result == c'SUCCESS
      then do
        return (treeFromPtr ptr)
      else do
        print $ "Error: " ++ (show result)
        return t

divideWithDevBigger :: Int -> ImageTree StatColor -> ImageTree StatColor
divideWithDevBigger _ EmptyTree = EmptyTree
divideWithDevBigger n t@(ImageTree _ b EmptyTree EmptyTree EmptyTree EmptyTree)
  | d > n     = divideTree t
  | otherwise = t
  where d = statDev . statColor . value $ b
divideWithDevBigger n (ImageTree p b tnw tne tsw tse) =
  (ImageTree p b
    (divideWithDevBigger n tnw)
    (divideWithDevBigger n tne)
    (divideWithDevBigger n tsw)
    (divideWithDevBigger n tse))

divideForest :: ImageForest StatColor -> ImageForest StatColor
divideForest NullForest = NullForest
divideForest (ImageForest ptr i r c ts) =
  (ImageForest ptr i r c (mapDeep divideTree ts))

divideTree :: ImageTree StatColor -> ImageTree StatColor
divideTree EmptyTree = EmptyTree
divideTree (ImageTree ptr _ EmptyTree EmptyTree EmptyTree EmptyTree) =
  unsafePerformIO $ do
    result <- c'image_tree_divide ptr
    if result == c'SUCCESS
      then do
        return $ treeFromPtr ptr
      else do
        return $ EmptyTree
divideTree t = t

treeFromListItem :: Ptr C'list_item -> ImageTree StatColor
treeFromListItem i_ptr =
  unsafePerformIO $ do
    i <- peek i_ptr
    return $ treeFromPtr $ castPtr $ c'list_item'data i

findTreeNeighbors :: ImageTree a -> [ImageTree StatColor]
findTreeNeighbors (ImageTree t_ptr _ _ _ _ _) =
  unsafePerformIO $ do
    flist <- allocList
    if isNothing flist
      then do
        print $ "Failed to allocate list"
        return $ []
      else
        withForeignPtr (fromJust flist) $ \l_ptr -> do
          result <- c'image_tree_find_all_immediate_neighbors l_ptr t_ptr
          if result == c'SUCCESS
            then
              return $! hlist $ createList (fromJust flist) treeFromListItem
            else
              return []

filterForest :: (a -> Bool) -> ImageForest a -> ImageForest a
filterForest cond (ImageForest ptr i r c ts) =
  (ImageForest ptr i r c (filter (cond . value . block) $! ts))



instance (Directed a) => Directed (ImageBlock a) where
toDir ImageBlock{ value = v } = toDir v

blockStat :: (StatColor -> Stat) -> ImageBlock StatColor -> Stat
blockStat f b = f . value $ b

blockMean :: (StatColor -> Stat) -> ImageBlock StatColor -> Int
blockMean f b = statMean . f . value $ b

blockStatMean :: ImageBlock Stat -> Int
blockStatMean b = statMean . value $ b

blockStatColorMean :: ImageBlock StatColor -> Int
blockStatColorMean b = statColorMean . value $ b

blockDev :: (StatColor -> Stat) -> ImageBlock StatColor -> Int
blockDev f b = statDev . f . value $ b

-- TODO: find out why the pair becomes inverted at some point..
-- color1 should be first, but the color is wrong if they are not inverted here
blockColorPair :: ImageBlock StatColor -> (Int, Int)
blockColorPair b = (statMean . statColor2 . value $ b, statMean . statColor1 . value $ b)
-- | d == nullDir = (0,0)
-- | otherwise = (statMean . statColor1 . value . block $ t, statMean . statColor2 . value . block $ t)
-- where
--   d = treeDir statColor t

-- for calculating directions, means from four subtrees are needed.
treeDivideMean :: (StatColor -> Stat) -> ImageTree StatColor -> DivideMean
treeDivideMean _ EmptyTree = (0,0,0,0)
treeDivideMean f (ImageTree _ _ tnw tne tsw tse) =
  ((blockMean f (block tnw)),(blockMean f (block tne)),(blockMean f (block tsw)),(blockMean f (block tse)))

treeStatColorToStatDir :: (StatColor -> Stat) -> ImageTree StatColor -> ImageTree StatDir
treeStatColorToStatDir _ EmptyTree = EmptyTree
treeStatColorToStatDir f (ImageTree p b EmptyTree EmptyTree EmptyTree EmptyTree) =
  (ImageTree p (fmap (nullStatDir . f) b) EmptyTree EmptyTree EmptyTree EmptyTree)
  treeStatColorToStatDir f t@(ImageTree p b tnw tne tsw tse) =
    (ImageTree p
    (fmap ((statDir (treeDivideMean f t)) . f) b)
    (fmap (nullStatDir . f) tnw)
    (fmap (nullStatDir . f) tne)
    (fmap (nullStatDir . f) tsw)
    (fmap (nullStatDir . f) tse))

treeStatColorToStatDirColor :: ImageTree StatColor -> ImageTree StatDirColor
treeStatColorToStatDirColor EmptyTree = EmptyTree
treeStatColorToStatDirColor (ImageTree p b EmptyTree EmptyTree EmptyTree EmptyTree) =
  (ImageTree p (fmap nullStatDirColor b) EmptyTree EmptyTree EmptyTree EmptyTree)
treeStatColorToStatDirColor t@(ImageTree p b tnw tne tsw tse) =
  (ImageTree p
  (fmap (statDirColor (treeDivideMean statColor t)
  (treeDivideMean statColor1 t)
  (treeDivideMean statColor2 t)) b)
  (fmap nullStatDirColor tnw)
  (fmap nullStatDirColor tne)
  (fmap nullStatDirColor tsw)
  (fmap nullStatDirColor tse))


-- TODO: min deviation for considering a tree as 'edgy' is hardcoded.
-- should move this out perhaps into a parameter.
treeDir :: (StatColor -> Stat) -> ImageTree StatColor -> Dir
treeDir _ EmptyTree = Dir(0,0)
treeDir _ (ImageTree _ _ EmptyTree EmptyTree EmptyTree EmptyTree) = Dir(0,0)
treeDir f t@(ImageTree _ _ tnw tne tsw tse)
  | (blockDev f (block t)) < 10 = Dir(0,0)
  | otherwise   = calculateDir (blockMean f (block tnw))
  (blockMean f (block tne))
  (blockMean f (block tsw))
  (blockMean f (block tse))

treeStatDir :: (StatColor -> Stat) -> ImageTree StatColor -> StatDir
treeStatDir f t = StatDir(blockStat f (block t),treeDir f t)
