{-# LANGUAGE OverlappingInstances, TupleSections, ScopedTypeVariables #-}

module CVSU.ImageTree
( ImageBlock(..)
, ImageTree(..)
, ImageForest(..)
, ForestValue(..)
, ForestRegion(..)
, withForest
, mapDeep
, filterForest
, treeWidth
, treeHeight
, treeChildStatistics
, treeDivide
, treeNeighbors
, treeClassFind
, treeClassUnion
, forestSegment
, forestSegmentDeviation
, forestSegmentEntropy
, forestRegionsGet
, forestDrawImage
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
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Concurrent
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
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int,
    value :: !v
    } deriving Eq

instance (Show v) => Show (ImageBlock v) where
  show (ImageBlock bx by bw bh bv) =
    "(B "++(show bx)++","++(show by)++","++(show bw)++","++(show bh)++","++(show bv)++")"

data ForestRegion =
  ForestRegion
  { regionId :: Integer
  , regionX :: Int
  , regionY :: Int
  , regionW :: Int
  , regionH :: Int
  , regionStat :: Statistics
  , regionColor :: (Float,Float,Float)
  }

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

treeWidth = w . block

treeHeight = h . block

instance (Show a) => Show (ImageTree a) where
  show EmptyTree = "(B)"
  show ImageTree{ block = b } = show b

data ImageForest v =
  ImageForest
  { forestPtr :: !(ForeignPtr C'image_tree_forest)
  , img :: PixelImage
  , rows :: Int
  , cols :: Int
  , regions :: Int
  , blockType :: ImageBlockType
  , trees :: ![ImageTree v]
  }

instance (Show a) => Show (ImageForest a) where
  show (ImageForest p _ r c _ _ ts) = "(F " ++
    (show r) ++ "x" ++ (show c) ++ " " ++ (show ts) ++ ")"

instance Eq (ImageForest a) where
  (==) f1 f2 = (forestPtr f1) == (forestPtr f2)

-- make ImageBlock a functor and applicative to allow fmapping

instance Functor ImageBlock where
  fmap f (ImageBlock bx by bw bh bv) = (ImageBlock bx by bw bh (f bv))

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
  fmap f (ImageForest ptr i r c s t ts) = (ImageForest ptr i r c s t (mapDeep (fmap f) ts))

class ForestValue a where
  -- | Ensure that the data type contained in the forest is correct
  checkType :: ImageForest a -> Bool

  -- | Cast a void pointer to the correct type contained in the forest
  toValue :: Ptr() -> IO (a)

  -- | Extract the given root tree from the forest
  getTree :: ImageForest a -> (Int,Int) -> IO (ImageTree a)
  getTree f (x,y) =
    withForeignPtr (forestPtr f) $ \pforest -> do
      forest <- peek pforest
      root <- peek $ advancePtr (c'image_tree_forest'roots forest) (y * (cols f) + x)
      (treeFromPtr toValue) $ treePtrFromRoot root

  -- | Initialize the forest structure, given an image and number of rows and
  --   columns. The init stage is needed to better reuse code between different
  --   type class implementations. Usually only init is implemented, and the
  --   create function can be shared.
  initForest :: PixelImage -> (Int,Int) -> IO (ImageForest a)

  -- | Creates the forest structure, given an image and number of rows and
  --   columns in the root tree array.
  createForest :: PixelImage -> (Int,Int) -> IO (ImageForest a)
  createForest i (w,h) = do
    s <- initForest i (w,h)
    if not $ checkType s
       then error "forest content type is invalid"
       else updateForest s

  -- | Refreshes the forest structure by reading it in full from the underlying
  --   c structure. Required after operations causing side effects to ensure
  --   consistency of the whole data structure.
  refreshForest :: ImageForest a -> IO (ImageForest a)
  refreshForest (ImageForest fforest i r c _ t _) =
    withForeignPtr fforest $ \pforest -> do
      C'image_tree_forest{
        c'image_tree_forest'roots=proots,
        c'image_tree_forest'regions=regions
      } <- peek pforest
      rs <- peekArray (r*c) proots
      ts <- mapM (treeFromPtr toValue) $ map treePtrFromRoot rs
      return $ ImageForest fforest i r c (fromIntegral regions) t ts

  -- | Updates the underlying integral images of the forest, invalidating the
  --   tree contents; thus it requires a refresh. Can be used for processing
  --   video, if the video frame buffer content is copied directly to the
  --   memory block containing the image data used for creating the forest.
  --   Also used by the create function.
  updateForest :: ImageForest a -> IO (ImageForest a)
  updateForest f@ImageForest{forestPtr=fforest} = do
    withForeignPtr fforest $ \pforest -> do
      result <- c'image_tree_forest_update pforest
      if result /= c'SUCCESS
         then error $ "Forest update failed with " ++ (show result)
         else refreshForest f

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

instance ForestValue Statistics where
  checkType = (==BlockStatistics).blockType
  initForest = createForestStub BlockStatistics
  toValue p = do
    stat <- peek ((castPtr p)::Ptr C'statistics)
    return $ hStatistics stat

createForestStub :: (ForestValue v) => ImageBlockType -> PixelImage -> (Int,Int) -> IO (ImageForest v)
createForestStub t i (w,h) = do
  fforest <- allocImageForest
  withForeignPtr fforest $ \pforest ->
    withForeignPtr (imagePtr i) $ \pimage -> do
      r1 <- c'image_tree_forest_create pforest pimage
        (fromIntegral w) (fromIntegral h) (cImageBlockType t)
      if r1 /= c'SUCCESS
        then error $ "Create forest failed with " ++ (show r1)
        else do
          C'image_tree_forest{
            c'image_tree_forest'rows = r,
            c'image_tree_forest'cols = c,
            c'image_tree_forest'regions = s,
            c'image_tree_forest'type = t
          } <- peek pforest
          return $ ImageForest fforest i
            (fromIntegral r)
            (fromIntegral c)
            (fromIntegral s)
            (hImageBlockType t)
            []

treePtrFromRoot :: C'image_tree_root -> Ptr C'image_tree
treePtrFromRoot r = c'image_tree_root'tree r

treeFromPtr :: (ForestValue v) => (Ptr() -> IO v) -> Ptr C'image_tree -> IO (ImageTree v)
treeFromPtr conv ptree
  | ptree == nullPtr = return EmptyTree
  | otherwise        = do
    tree <- peek ptree
    cid <- c'image_tree_class_get ptree
    (C'image_block x y w h pvalue) <- peek (c'image_tree'block tree)
    v <- conv pvalue
    tnw <- treeFromPtr conv (c'image_tree'nw tree)
    tne <- treeFromPtr conv (c'image_tree'ne tree)
    tsw <- treeFromPtr conv (c'image_tree'sw tree)
    tse <- treeFromPtr conv (c'image_tree'se tree)
    let b = ImageBlock (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) v
    return $ ImageTree ptree (fromIntegral cid) b tnw tne tsw tse

-- allocate image_tree_forest structure using c function and foreign pointers
-- image_tree_forest_alloc is used for allocating the image struct
-- image_tree_forest_free is specified as finalizer
allocImageForest :: IO (ForeignPtr C'image_tree_forest)
allocImageForest = do
  ptr <- c'image_tree_forest_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'image_tree_forest_free ptr)
    else error "Memory allocation failed in allocImageForest"

withForest :: ImageForest a -> (ImageForest a -> IO b) -> IO b
withForest f op =
  withForeignPtr (forestPtr f) $ \_ -> do
    r <- op f
    touchForeignPtr (forestPtr f)
    return $! r

deep :: NFData a => a -> a
deep a = deepseq a a

instance NFData (ImageTree a) where
  rnf (ImageTree _ _ b tnw tne tsw tse) = tnw `seq` tne `seq` tsw `seq` tse `seq` b `seq` ()

instance NFData (ImageBlock a) where
  rnf (ImageBlock bx by bw bh bv) = bx `seq` by `seq` bw `seq` bh `seq` bv `seq` ()

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
filterForest cond (ImageForest ptr i r c s t ts) =
  (ImageForest ptr i r c s t (filter (cond . value . block) $! ts))

treeChildStatistics :: ImageTree a -> IO [Statistics]
treeChildStatistics t = do
  let
    allocStatArray :: IO (Ptr C'statistics)
    allocStatArray = mallocArray 4
  pstat <- allocStatArray
  r <- c'image_tree_get_child_statistics (treePtr t) pstat nullPtr
  if r /= c'SUCCESS
    then error $ "Get child statistics failed with " ++ (show r)
    else do
      stat <- (peekArray 4 pstat)
      return $ map hStatistics stat

-- | Divides the tree in four equal parts in quad-tree fashion. Causes side
--   effects as the original tree will have four child trees after this
--   operation.
treeDivide :: (ForestValue b) => ImageTree a -> IO [ImageTree b]
treeDivide t = do
  r <- c'image_tree_divide (treePtr t)
  if (r /= c'SUCCESS)
    then error $ "Divide tree failed with " ++ (show r)
    else do
      ct <- peek (treePtr t)
      mapM (treeFromPtr toValue)
        [ c'image_tree'nw ct
        , c'image_tree'ne ct
        , c'image_tree'sw ct
        , c'image_tree'se ct ]

-- | Creates an image tree from a list item by casting and converting
treeFromListItem :: (ForestValue a) => Ptr C'list_item -> IO (ImageTree a)
treeFromListItem pitem = do
  item <- peek pitem
  ptree <- peek $ castPtr $ c'list_item'data item
  treeFromPtr toValue $ castPtr $ ptree

-- | Finds the current neighbors of the given tree.
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

-- | Initializes a disjunctive set for the tree by setting the parent to
--   self and rank to zero.
treeClassInit :: ImageTree a -> IO (ImageTree a)
treeClassInit t@(ImageTree ptr _ b nw ne sw se) = do
  c'image_tree_class_create ptr
  cid <- treeClassFind t
  return $ ImageTree ptr cid b nw ne sw se

-- | Union part of the Union-Find disjunctive set algorithm. Creates a union
--   of two sets by assigning the parent of the higher ranking tree as the
--   parent of the union. As the result, the two trees in question will have
--   the same classId. Loses persistency, as the classId is changed in the
--   underlying c structure, so previous versions of the trees that still
--   point to the same c structure will have different classId.
treeClassUnion :: (ImageTree a, ImageTree a) -> IO (ImageTree a, ImageTree a)
treeClassUnion (t1@(ImageTree ptr1 _ b1 nw1 ne1 sw1 se1),
                t2@(ImageTree ptr2 _ b2 nw2 ne2 sw2 se2)) = do
  c'image_tree_class_union ptr1 ptr2
  cid1 <- treeClassFind t1
  cid2 <- treeClassFind t2
  return $! ((ImageTree ptr1 cid1 b1 nw1 ne1 sw1 se1),
          (ImageTree ptr2 cid2 b2 nw2 ne2 sw2 se2))

-- | Find part of the Union-Find disjunctive set algorithm. Finds the pointer
--   of the parent tree, caches it in the c structure, and creates an Integer
--   from the pointer. May cause side effects as the parent pointer is
--   cached to shorten the search paths for better efficiency.
treeClassFind :: ImageTree a -> IO (Integer)
treeClassFind t = do
  tid <- c'image_tree_class_get (treePtr t)
  return $ fromIntegral tid

-- | Segments a forest by dividing trees until they are consistent by the
--   given measure, and by grouping neighboring trees, that are deemed equal in
--   terms of the provided equivalence operation, by using equivalence classes
--   implemented for image trees using Union-Find algorithm. The result is a
--   segmented forest where trees belonging to the same region have the same
--   classId. Note: the treeDivide and treeClassUnion operations cause side
--   effects, so this whole operation must be performed in full to leave the
--   tree list in consistent state, and in the current form, the algorithm is
--   not safe to parallelize (though in future it probably will be). Also the
--   resulting Haskell data structure is not persistent (may be later).
forestSegment :: (ForestValue a) => Int -> (ImageTree a -> Bool)
  -> (ImageTree a -> ImageTree a -> Bool) -> ImageForest a -> IO (ImageForest a)
forestSegment minSize isConsistent isEq f = do
  ts <- segment $ trees f
  ts `seq` refreshForest f
  where
    -- find all neighbors and create a union with those that are equivalent and
    -- consistent according to given measures
    --unionWithNeighbors :: (ForestValue a) => ImageTree a -> IO [(ImageTree a, ImageTree a)]
    unionWithNeighbors t = do
      ns :: [ImageTree a] <- treeNeighbors t
      mapM_ (treeClassUnion.(t,)) $ filter (\n -> (isConsistent n) && (isEq t n)) ns
    -- go through all trees; add consistent ones to segments, divide in four
    -- the inconsistent ones
    --segment :: (ForestValue a) => [ImageTree a] -> IO ()
    segment [] = return ()
    segment (t:ts)
      | isConsistent t = do
        rs <- unionWithNeighbors t
        rs `seq` segment ts
      | treeWidth t > minSize = do
        cs <- treeDivide t
        cs `seq` segment (ts ++ cs)
      | otherwise = segment ts

forestSegmentDeviation :: (ForestValue a) => Double -> Int 
  -> ImageForest a -> IO (ImageForest a)
forestSegmentDeviation t minSize f =
  withForeignPtr (forestPtr f) $ \pforest -> do
    r <- c'image_tree_forest_segment_with_deviation pforest (realToFrac t) (fromIntegral minSize)
    if r /= c'SUCCESS
       then error $ "forestSegmentDeviation failed with " ++ (show r)
       else refreshForest f

forestSegmentEntropy :: (ForestValue a) => Int -> ImageForest a -> IO (ImageForest a)
forestSegmentEntropy minSize f =
  withForeignPtr (forestPtr f) $ \pforest -> do
    r <- c'image_tree_forest_segment_with_entropy pforest (fromIntegral minSize)
    if r /= c'SUCCESS
       then error $ "forestSegmentEntropy failed with " ++ (show r)
       else refreshForest f

forestRegionsGet :: (ForestValue a) => ImageForest a -> IO [ForestRegion]
forestRegionsGet f =
  withForeignPtr (forestPtr f) $ \pforest -> do
    let
      targetSize = regions f
      allocTargetArray :: IO (Ptr (Ptr C'forest_region_info))
      allocTargetArray = mallocArray targetSize
      makeC = (/255).realToFrac
      toColor c1 c2 c3 = (makeC c1, makeC c2, makeC c3)
      readRegion :: Ptr C'forest_region_info -> IO (ForestRegion)
      readRegion pregion = do
        (C'forest_region_info rid _ x1 y1 x2 y2 stat c1 c2 c3) <- peek pregion
        return $ ForestRegion
          (fromIntegral $ ptrToWordPtr rid)
          (fromIntegral x1)
          (fromIntegral y1)
          (fromIntegral $ x2-x1)
          (fromIntegral $ y2-y1)
          (hStatistics stat)
          (toColor c1 c2 c3)
    ptarget <- allocTargetArray
    c'image_tree_forest_get_regions pforest ptarget
    target <- (peekArray targetSize ptarget)
    mapM readRegion target

-- | Draws an image of the forest using the current division and region info.
--   Information from regions or individual trees will be used (based on
--   parameter useRegions) and either the mean intensity of the whole region or
--   the assigned color of the region can be used (based on parameter useColors).
forestDrawImage :: Bool -> Bool -> ImageForest a -> IO (PixelImage)
forestDrawImage useRegions useColors forest = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withForeignPtr (forestPtr forest) $ \pforest -> do
      r <- c'image_tree_forest_draw_image 
        pforest
        pimg
        (if useRegions then 1 else 0)
        (if useColors then 1 else 0)
      if r /= c'SUCCESS
         then error "Drawing forest image failed"
         else ptrToPixelImage True fimg
