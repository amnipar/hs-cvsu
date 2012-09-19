{-# LANGUAGE OverlappingInstances #-}

module CVSU.ImageTree
( Stat(..)
, Dir(..)
, StatDir(..)
, StatColor(..)
, DirColor(..)
, StatDirColor(..)
, Mean(..)
, DivideMean(..)
, nullDir
, dir
, nullStatDir
, nullDirColor
, nullStatDirColor
, statMean
, statDev
, statDir
, statDirMean
, statColor
, statColor1
, statColor2
, statColorMean
, statColorMean1
, statColorMean2
, statDirColor
, statDirColorMean
, statDirColorMean1
, statDirColorMean2
, ImageBlock(..)
, ImageTree(..)
, ImageForest(..)
, Directed(..)
, blockStat
, blockMean
, blockStatMean
, blockStatColorMean
, blockDev
, blockColorPair
, treeDivideMean
, treeStatColorToStatDir
, treeStatColorToStatDirColor
, treeDir
, treeStatDir
, readImageForest
, readCVImageForest
, withForest
, withForestFromImage
, withForestFromGreyCVImage
, withForestFromColorCVImage
, forestImage
, forestSize
, touchForest
, updateForest
, mapDeep
, updateTree
, divideForest
, divideTree
, filterForest
, colorPairToUV
--, uvToColor
, forestGeometry
, normalize
, treeToBlockWithFeatureVector
, forestToBlocksWithFeatureVectors
, cvColorImageToColorPairs
, cvColorImageToFeatureVector
, fileToFeatureVector
, analyzeForest
, objectCenter
, objectGeometry
, objectFeature
, Direction(..)
, DirectionDistance(..)
, CenterPoint
, Geometry
, GeometryFeature
, EdgeType(..)
, ColorName(..)
, ColorPair(..)
, ObjectColor(..)
, ObjectInfo(..)
, ImageObject(..)
, geometry4
, geometry8
, directionVector
, directionScale
, blockDirectionDistance
, roadSignColors
) where

import CVSU.Bindings.Types
import CVSU.Bindings.PixelImage
import CVSU.Bindings.List
import CVSU.Bindings.OpenCV
import CVSU.Bindings.ImageTree
import CVSU.PixelImage

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

newtype Stat = Stat(Int, Int) deriving (Eq, Show)
newtype Dir = Dir(Int, Int) deriving (Eq, Show)
newtype StatDir = StatDir(Stat, Dir) deriving (Eq, Show)
newtype StatColor = StatColor(Stat, Stat, Stat) deriving (Eq, Show)
newtype DirColor = DirColor(Dir, Dir, Dir) deriving (Eq, Show)
newtype StatDirColor = StatDirColor(StatDir, StatDir, StatDir) deriving (Eq, Show)

type Mean = Int
type DivideMean = (Int, Int, Int, Int)
type ColorMean = (Int, Int, Int)

nullDir :: Dir
nullDir = Dir(0,0)

dir :: DivideMean -> Dir
dir (mnw, mne, msw, mse) = Dir((mnw + msw) - (mne + mse), (mnw + mne) - (msw + mse))

nullStatDir :: Stat -> StatDir
nullStatDir s = StatDir(s,nullDir)

nullDirColor :: DirColor
nullDirColor = DirColor(nullDir,nullDir,nullDir)

nullStatDirColor :: StatColor -> StatDirColor
nullStatDirColor (StatColor(s1,s2,s3)) = StatDirColor(nullStatDir s1, nullStatDir s2, nullStatDir s3)

statMean :: Stat -> Mean
statMean (Stat(m,_)) = m

statDev :: Stat -> Int
statDev (Stat(_,d)) = d

statDir :: DivideMean -> Stat -> StatDir
statDir m s = StatDir(s,(dir m))

statDirMean :: StatDir -> Mean
statDirMean (StatDir(Stat(m,_),_)) = m

statColor :: StatColor -> Stat
statColor (StatColor(s,_,_)) = s

statColor1 :: StatColor -> Stat
statColor1 (StatColor(_,s1,_)) = s1

statColor2 :: StatColor -> Stat
statColor2 (StatColor(_,_,s2)) = s2

statColorMean :: StatColor -> Mean
statColorMean (StatColor(Stat(m,_),_,_)) = m

statColorMean1 :: StatColor -> Mean
statColorMean1 (StatColor(_,Stat(m1,_),_)) = m1

statColorMean2 :: StatColor -> Mean
statColorMean2 (StatColor(_,_,Stat(m2,_))) = m2

statDirColor :: DivideMean -> DivideMean -> DivideMean -> StatColor -> StatDirColor
statDirColor m m1 m2 (StatColor(s,s1,s2)) =
  StatDirColor((statDir m s),(statDir m1 s1),(statDir m2 s2))

statDirColorMean :: StatDirColor -> Mean
statDirColorMean (StatDirColor(s,_,_)) = statDirMean s

statDirColorMean1 :: StatDirColor -> Mean
statDirColorMean1 (StatDirColor(_,s1,_)) = statDirMean s1

statDirColorMean2 :: StatDirColor -> Mean
statDirColorMean2 (StatDirColor(_,_,s2)) = statDirMean s2

data ImageBlock v =
  ImageBlock{
    n :: Float,
    e :: Float,
    s :: Float,
    w :: Float,
    value :: !v
    } deriving Eq

data ImageTree v = EmptyTree |
  ImageTree
  { treePtr :: Ptr C'image_tree
  , block :: !(ImageBlock v)
  , nw :: ImageTree v
  , ne :: ImageTree v
  , sw :: ImageTree v
  , se :: ImageTree v
  } deriving Eq

data ImageForest v =
  NullForest |
  ImageForest {
    forestPtr :: !(ForeignPtr C'image_tree_forest),
    img :: PixelImage,
    rows :: Int,
    cols :: Int,
    trees :: ![ImageTree v]
    }

--instance (Eq a) => Eq (ImageBlock a) where
--  (==) (ImageBlock na ea sa wa va) (ImageBlock nb eb sb wb vb) =
--    na == nb && ea == eb && sa == sb && wa == wb && va == vb

--instance (Eq a) => Eq (ImageTree a) where
--(==) (ImageTree pa ba nwa nea swa sea) (ImageTree pb bb nwb neb swb seb) =

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
  fmap f (ImageForest ptr i r c ts) = (ImageForest ptr i r c (map (fmap f) ts))

class Directed a where
  toDir :: a -> Dir

instance Directed Stat where
  toDir a = nullDir

instance Directed StatDir where
  toDir (StatDir(_,d)) = d

instance Directed StatColor where
  toDir a = nullDir

instance Directed DirColor where
  toDir (DirColor(d,_,_)) = d

instance Directed StatDirColor where
  toDir (StatDirColor(StatDir(_,d),_,_)) = d

instance (Directed a) => Directed (ImageBlock a) where
  toDir ImageBlock{ value = v } = toDir v

instance (Show v) => Show (ImageBlock v) where
  show (ImageBlock bn be bs bw bv) =
    "("++(show bn)++","++(show be)++","++(show bs)++","++(show bw)++","++(show bv)++")"

instance (Show a) => Show (ImageTree a) where
  show EmptyTree = ""
  show ImageTree{ block = b } = show b

instance (Show a) => Show (ImageForest a) where
  show NullForest = "(NullForest)"
  show (ImageForest p i r c ts) = "(ImageForest " ++
    (show r) ++ "x" ++ (show c) ++ " " ++ (show ts) ++ ")"

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

-- direction of 'edgeness' within tree is calculated as a relation between
-- subtree means on top and bottom (h) and left and right (v) sides.
-- TODO: differences must be close enough to consider an edge diagonal.
-- should move this check elsewhere, perhaps filtering directions at later stage.
calculateDir :: Int -> Int -> Int -> Int -> Dir
calculateDir mnw mne msw mse
  | (abs 3*h) < (abs v) = Dir(0,v)
  | (abs 3*v) < (abs h) = Dir(h,0)
  | otherwise           = Dir(h,v)
  where
    h = (mnw + mne) - (msw + mse)
    v = (mnw + msw) - (mne + mse)

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

treeFromPtr :: Ptr C'image_tree -> ImageTree StatColor
treeFromPtr ptr
  | ptr == nullPtr = EmptyTree
  | otherwise    = unsafePerformIO $ do
    t <- peek ptr
    C'image_block{
    c'image_block'x = x,
    c'image_block'y = y,
    c'image_block'w = w,
    c'image_block'h = h,
    c'image_block'value =
      C'stat_color{
      c'stat_color'mean_i = m,
      c'stat_color'dev_i = d,
      c'stat_color'mean_c1 = m1,
      c'stat_color'dev_c1 = d1,
      c'stat_color'mean_c2 = m2,
      c'stat_color'dev_c2 = d2
      }
    } <- peek (c'image_tree'block t)
    --print $ (show m) ++ " " ++ (show d)
    return $
      ImageTree
      { treePtr = ptr
      , block =
        ImageBlock
        { n = (fromIntegral y)
        , e = (fromIntegral x) + (fromIntegral w)
        , s = (fromIntegral y) + (fromIntegral h)
        , w = (fromIntegral x)
        , value = (StatColor(Stat((fromIntegral m),(fromIntegral d)),
                             Stat((fromIntegral m1),(fromIntegral d1)),
                             Stat((fromIntegral m2),(fromIntegral d2))))
        --, value = Statistics{ mean = (fromIntegral m), dev = (fromIntegral d) }
        }
      , nw = treeFromPtr $ c'image_tree'nw t
      , ne = treeFromPtr $ c'image_tree'ne t
      , sw = treeFromPtr $ c'image_tree'sw t
      , se = treeFromPtr $ c'image_tree'se t
      }

rootToImageTree :: C'image_tree_root -> ImageTree StatColor
rootToImageTree r = treeFromPtr (c'image_tree_root'tree r)

forestFromPtr :: ForeignPtr C'image_tree_forest -> IO (ImageForest StatColor)
forestFromPtr ptr = do
  withForeignPtr ptr $ \fPtr -> do
    C'image_tree_forest{
      c'image_tree_forest'original = i_ptr,
      c'image_tree_forest'rows = r,
      c'image_tree_forest'cols = c,
      c'image_tree_forest'roots = rPtr
    } <- peek fPtr
    poke (p'image_tree_forest'own_original fPtr) 0
    --i <- ptrToPixelImage True i_ptr
    rs <- peekArray ((fromIntegral r) * (fromIntegral c)) rPtr
    return $
      ImageForest
      { forestPtr = ptr
      , img = NullImage
      , rows = (fromIntegral r)
      , cols = (fromIntegral c)
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

withForestFromImage :: String -> Int -> Int -> (ImageForest StatColor -> b) -> IO b
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



withForestFromGreyCVImage :: (Image GrayScale D8) -> Int -> Int -> (ImageForest StatColor -> b) -> IO b
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
                fresult <- c'image_tree_forest_create f_ptr i_ptr (fromIntegral w) (fromIntegral h)
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
                fresult <- c'image_tree_forest_create f_ptr i_ptr (fromIntegral w) (fromIntegral h)
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

readImageForest :: String -> Int -> Int -> IO (Maybe (ImageForest StatColor))
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

readCVImageForest :: Image RGB D8 -> Int -> Int -> IO (ImageForest StatColor)
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
                fresult <- c'image_tree_forest_create f_ptr i_ptr (fromIntegral w) (fromIntegral h)
                if fresult == c'SUCCESS
                  then do
                    forestFromPtr $ fromJust imgf
                  else do
                    print $ "Error: failed to create image forest"
                    return $ NullForest
            else do
              print $ "Error: failed to create pixel image from CV image"
              return $ NullForest

-- TODO: should perhaps make a clone of the image instead of repossessing it
-- this will cause problems if same forest structure is used with multiple images
-- (like an image sequence or a video stream)
forestImage :: (ImageForest a) -> IO (PixelImage)
forestImage f =
  withForeignPtr (forestPtr f) $ \f_ptr -> do
    C'image_tree_forest{
      c'image_tree_forest'original = i_ptr
    } <- peek f_ptr
    poke (p'image_tree_forest'own_original f_ptr) 0
    return NullImage -- ptrToPixelImage True i_ptr

--forestWithImage :: (ImageForest a) -> (ImageForest a)
--forestWithImage f@(ImageForest ptr i r c ts)
--  | i /= NullImage = f
--  | otherwise = (ImageForest ptr (lift $ forestImage f) r c ts)

forestSize :: (ImageForest a) -> IO (Int, Int)
forestSize f =
  withForeignPtr (forestPtr f) $ \f_ptr -> do
    i_ptr <- peek $ p'image_tree_forest'original f_ptr
    w <- peek $ p'pixel_image'width i_ptr
    h <- peek $ p'pixel_image'height i_ptr
    return (fromIntegral w, fromIntegral h)

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

instance NFData Stat where
  rnf (Stat(m,d)) = m `seq` d `seq` ()

instance NFData Dir where
  rnf (Dir(h,v)) = h `seq` v `seq` ()

instance NFData StatDir where
  rnf (StatDir(s,d)) = s `seq` d `seq` ()

instance NFData StatColor where
  rnf (StatColor(s,s1,s2)) = s `seq` s1 `seq` s2 `seq` ()

instance NFData DirColor where
  rnf (DirColor(d,d1,d2)) = d `seq` d1 `seq` d2 `seq` ()

instance NFData StatDirColor where
  rnf (StatDirColor(s,s1,s2)) = s `seq` s1 `seq` s2 `seq` ()

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
divideWithDevBigger _ t = t

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

filterForest :: (a -> Bool) -> ImageForest a -> ImageForest a
filterForest cond (ImageForest ptr i r c ts) =
  (ImageForest ptr i r c (filter (cond . value . block) $! ts))

boundToByte :: Float -> Int
boundToByte d
  | i < 0     = 0
  | i > 255   = 255
  | otherwise = i
  where i = floor d

colorPairToUV :: (Int, Int) -> (Float, Float)
colorPairToUV (c1,c2) =
  (u,v)
  where
        u = ((fromIntegral c1 / 255.0) * 2.0 * 0.436) - 0.436
        v = ((fromIntegral c2 / 255.0) * 2.0 * 0.615) - 0.615

--uvToColor :: (Float, Float) -> Color
--uvToColor (u,v) =
--  makeColor r g b 1.0
--  where
--        r = (0.75 + 0.00000 * u + 1.13983 * v)
--        g = (0.75 - 0.39465 * u - 0.58060 * v)
--        b = (0.75 + 2.03211 * u + 0.00000 * v)

-- r = boundToByte $ 255.0 * (0.5 + 0.00000 * u + 1.13983 * v)
-- g = boundToByte $ 255.0 * (0.5 - 0.39465 * u - 0.58060 * v)
-- b = boundToByte $ 255.0 * (0.5 + 2.03211 * u + 0.00000 * v)

cvColorImageToColorPairs :: String -> [(Int, Int)]
cvColorImageToColorPairs s =
  unsafePerformIO $ do
    i <- readFromFile s
    withForestFromColorCVImage i 8 8 $
      (mapDeep $ blockColorPair . block) . trees . divideForest . updateForest

blockDistance :: (Float, Float) -> (Float, Float) -> Float
blockDistance (ax,ay) (bx,by) =
  sqrt $ (ax-bx)**2.0 + (ay-by)**2.0

colorDistance :: (Int, Int) -> (Int, Int) -> Float
colorDistance (c1a,c2a) (c1b,c2b) =
  sqrt $ (fromIntegral c1a - fromIntegral c1b)**2.0 + (fromIntegral c2a - fromIntegral c2b)**2.0

isFlat :: Dir -> Double
isFlat d
  | d == nullDir = 1.0
  | otherwise    = 0.0

isEdge :: Dir -> Double
isEdge d
  | d == nullDir = 0.0
  | otherwise    = 1.0

isHorizontalEdge :: Dir -> Double
isHorizontalEdge (Dir(h,v))
  | (abs h) > (abs v) = 1.0 -- | h /= 0 && v == 0 = 1.0
  | otherwise         = 0.0

isVerticalEdge :: Dir -> Double
isVerticalEdge (Dir(h,v))
  | (abs v) > (abs h) = 1.0 -- | h == 0 && v /= 0 = 1.0
  | otherwise         = 0.0

isForwardEdge :: Dir -> Double
isForwardEdge (Dir(h,v))
  | (h > 0 && v > 0) || (h < 0 && v < 0)  = 1.0
  | otherwise                             = 0.0

isBackwardEdge :: Dir -> Double
isBackwardEdge (Dir(h,v))
  | (h > 0 && v < 0) || (h < 0 && v > 0) = 1.0
  | otherwise                            = 0.0

isBackground :: (Int, Int) -> Int -> Bool
isBackground c m
  | d < 16 && m > 96 && m < 160 = True
  | otherwise                   = False
  where
        d = colorDistance c (128,128)

isNotBackground :: (Int, Int) -> Int -> Double
isNotBackground c m
  | d > 24 || (m < 96 || m > 160) = 0.0
  | otherwise                   = 1.0
  where
        d = colorDistance c (128,128)

isRed :: (Int, Int) -> Double
isRed c
  | d < 24    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (128,240)

isYellow :: (Int, Int) -> Double
isYellow c
  | d < 24    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (24, 192)

isGreen :: (Int, Int) -> Double
isGreen c
  | d < 24    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (128, 80)

isDarkGreen :: (Int, Int) -> Double
isDarkGreen c
  | d < 24    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (64, 48)

isBlue :: (Int, Int) -> Double
isBlue c
  | d < 24    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (192,48)

isBrown :: (Int, Int) -> Double
isBrown c
  | d < 16    = 1.0
  | otherwise = 0.0
  where
        d = colorDistance c (104,184)

isBlack :: (Int, Int) -> Int -> Double
isBlack c m
  | d < 24 && m < 64 = 1.0
  | otherwise        = 0.0
  where
        d = colorDistance c (128,128)

isWhite :: (Int, Int) -> Int -> Double
isWhite c m
  | d < 24 && m > 192 = 1.0
  | otherwise         = 0.0
  where
        d = colorDistance c (128,128)

isTopLeftCenter :: ObjectGeometry -> (Float, Float) -> Double
isTopLeftCenter ((ox,oy),(on,_,_,ow)) (bx,by)
  | bx < ox && by < oy && (blockDistance (ox,oy) (bx,by)) < ((min (oy-on) (ox-ow)) / 2) = 1.0
  | otherwise = 0.0

isTopLeftRim :: ObjectGeometry -> (Float, Float) -> Double
isTopLeftRim ((ox,oy),(on,_,_,ow)) (bx,by)
  | bx < ox && by < oy && (blockDistance (ox,oy) (bx,by)) > ((min (oy-on) (ox-ow)) / 2) = 1.0
  | otherwise = 0.0

isTopRightCenter :: ObjectGeometry -> (Float, Float) -> Double
isTopRightCenter ((ox,oy),(on,oe,_,_)) (bx,by)
  | bx > ox && by < oy && (blockDistance (ox,oy) (bx,by)) < ((min (oy-on) (oe-ox)) / 2) = 1.0
  | otherwise = 0.0

isTopRightRim :: ObjectGeometry -> (Float, Float) -> Double
isTopRightRim ((ox,oy),(on,oe,_,_)) (bx,by)
  | bx > ox && by < oy && (blockDistance (ox,oy) (bx,by)) > ((min (oy-on) (oe-ox)) / 2) = 1.0
  | otherwise = 0.0

isBottomLeftCenter :: ObjectGeometry -> (Float, Float) -> Double
isBottomLeftCenter ((ox,oy),(_,_,os,ow)) (bx,by)
  | bx < ox && by > oy && (blockDistance (ox,oy) (bx,by)) < ((min (os-oy) (ox-ow)) / 2) = 1.0
  | otherwise = 0.0

isBottomLeftRim :: ObjectGeometry -> (Float, Float) -> Double
isBottomLeftRim ((ox,oy),(_,_,os,ow)) (bx,by)
  | bx < ox && by > oy && (blockDistance (ox,oy) (bx,by)) > ((min (os-oy) (ox-ow)) / 2) = 1.0
  | otherwise = 0.0

isBottomRightCenter :: ObjectGeometry -> (Float, Float) -> Double
isBottomRightCenter ((ox,oy),(_,oe,os,_)) (bx,by)
  | bx > ox && by > oy && (blockDistance (ox,oy) (bx,by)) < ((min (os-oy) (oe-ox)) / 2) = 1.0
  | otherwise = 0.0

isBottomRightRim :: ObjectGeometry -> (Float, Float) -> Double
isBottomRightRim ((ox,oy),(_,oe,os,_)) (bx,by)
  | bx > ox && by > oy && (blockDistance (ox,oy) (bx,by)) > ((min (os-oy) (oe-ox)) / 2) = 1.0
  | otherwise = 0.0

isAtLeft :: ObjectGeometry -> (Float, Float) -> Double
isAtLeft ((ox,oy),_) (bx,by)
  | bx < ox   = 1.0
  | otherwise = 0.0

isAtRight :: ObjectGeometry -> (Float, Float) -> Double
isAtRight ((ox,oy),_) (bx,by)
  | bx > ox   = 1.0
  | otherwise = 0.0

isAtTop :: ObjectGeometry -> (Float, Float) -> Double
isAtTop ((ox,oy),_) (bx,by)
  | by < oy   = 1.0
  | otherwise = 0.0

isAtBottom :: ObjectGeometry -> (Float, Float) -> Double
isAtBottom ((ox,oy),_) (bx,by)
  | by > oy   = 1.0
  | otherwise = 0.0

isAtCenter :: ObjectGeometry -> (Float, Float) -> Double
isAtCenter ((ox,oy),(on,oe,os,ow)) (bx,by)
  | bx < ox && bx > ow+(ox-ow)/3 && by < oy && by > on+(oy-on)/3 = 1.0
  | bx < ox && bx > ow+(ox-ow)/3 && by > oy && by < os-(os-oy)/3 = 1.0
  | bx > ox && bx < oe-(oe-ox)/3 && by < oy && by > on+(oy-on)/3 = 1.0
  | bx > ox && bx < oe-(oe-ox)/3 && by > oy && by < os-(os-oy)/3 = 1.0
  | otherwise = 0.0

isAtRim :: ObjectGeometry -> (Float, Float) -> Double
isAtRim ((ox,oy),(on,oe,os,ow)) (bx,by)
  | bx < ox && bx > ow+(ox-ow)/3 && by < oy && by > on+(oy-on)/3 = 0.0
  | bx < ox && bx > ow+(ox-ow)/3 && by > oy && by < os-(os-oy)/3 = 0.0
  | bx > ox && bx < oe-(oe-ox)/3 && by < oy && by > on+(oy-on)/3 = 0.0
  | bx > ox && bx < oe-(oe-ox)/3 && by > oy && by < os-(os-oy)/3 = 0.0
  | otherwise = 1.0

type ImagePoint = (Float, Float)
type CenterPoint = ImagePoint

-- spatial relationship calculations
-- normalize the centerpoint to (0,0)
-- move the inspected point so it's in correct direction relative to center
-- use dot product to determine the closest direction of 8 compass directions
-- reduce number of dot product calculations by checking in which quadrant
-- the point is (top left, top right, etc.)

data Direction =
  DirectionUnknown |
  DirectionC |
  DirectionNW |
  DirectionN |
  DirectionNE |
  DirectionE |
  DirectionSE |
  DirectionS |
  DirectionSW |
  DirectionW
  deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector d =
  case d of
    DirectionNW -> pNW
    DirectionN  -> pN
    DirectionNE -> pNE
    DirectionE  -> pE
    DirectionSE -> pSE
    DirectionS  -> pS
    DirectionSW -> pSW
    DirectionW  -> pW
    _           -> (0,0)

-- direction, distance in that direction, and associated point
-- used for example to store max distance in given direction
type DirectionDistance = (Direction, Float)

type Geometry = (CenterPoint, [DirectionDistance])

geometry4 =
  [ (DirectionN,  0.0)
  , (DirectionE,  0.0)
  , (DirectionS,  0.0)
  , (DirectionW,  0.0)
  ]

geometry8 =
  [ (DirectionNW, 0.0)
  , (DirectionN,  0.0)
  , (DirectionNE, 0.0)
  , (DirectionE,  0.0)
  , (DirectionSE, 0.0)
  , (DirectionS,  0.0)
  , (DirectionSW, 0.0)
  , (DirectionW,  0.0)
  ]

-- radial histogram with
-- eight directional bins
-- two distance bins (more / less than half the max dist)
-- two shape bins (edge / no edge)
radialHistogram822 =
  [ (DirectionNW, 0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionN,  0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionNE, 0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionE,  0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionSE, 0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionS,  0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionSW, 0.0, (0.0,0.0), (0.0,0.0))
  , (DirectionW,  0.0, (0.0,0.0), (0.0,0.0))
  ]

sqrt2 = sqrt 2.0

pNW = ((-sqrt2)/2.0, (-sqrt2)/2.0)
pN  = (0.0, (-1.0))
pNE = (sqrt2/2.0, (-sqrt2)/2.0)
pE  = (1.0, 0.0)
pSE = (sqrt2/2.0, (sqrt2)/2.0)
pS  = (0.0, (1.0))
pSW = ((-sqrt2)/2.0, (sqrt2)/2.0)
pW  = (-1.0, 0.0)

directionScale :: Float -> (Float, Float) -> (Float, Float)
directionScale s (x, y) = (s*x, s*y)

dotProduct :: (Float, Float) -> (Float, Float) -> Float
dotProduct (ax,ay) (bx,by) = ax*bx + ay*by

dotNW = dotProduct pNW
dotN  = dotProduct pN
dotNE = dotProduct pNE
dotE  = dotProduct pE
dotSE = dotProduct pSE
dotS  = dotProduct pS
dotSW = dotProduct pSW
dotW  = dotProduct pW

-- project a point to each cardinal direction
-- point belongs to that sector, where the projection magnitude is smallest
pointDirection8 :: ImagePoint -> ImagePoint -> DirectionDistance
pointDirection8 (cx,cy) p@(px,py)
-- check NW quadrant
  | rx < 0 && ry < 0 = dirNW r
-- check NE quadrant
  | rx > 0 && ry < 0 = dirNE r
-- check SW quadrant
  | rx < 0 && ry > 0 = dirSW r
-- check SE quadrant
  | rx > 0 && ry > 0 = dirSE r
-- if no direction found, assume it's in center
  | otherwise          = (DirectionC, 0.0)
  where
-- normalize point with center point as origin
    r@(rx,ry) = (px-cx, py-cy)
    dirNW a
      | (dotN a) > (dotW a) && (dotN a) > (dotNW a) = (DirectionN, dotN a)
      | (dotW a) > (dotN a) && (dotW a) > (dotNW a) = (DirectionW, dotW a)
      | otherwise = (DirectionNW, dotNW a)
    dirNE a
      | (dotN a) > (dotE a) && (dotN a) > (dotNE a) = (DirectionN, dotN a)
      | (dotE a) > (dotN a) && (dotE a) > (dotNE a) = (DirectionE, dotE a)
      | otherwise = (DirectionNE, dotNE a)
    dirSW a
      | (dotS a) > (dotW a) && (dotS a) > (dotSW a) = (DirectionS, dotS a)
      | (dotW a) > (dotS a) && (dotW a) > (dotSW a) = (DirectionW, dotW a)
      | otherwise = (DirectionSW, dotSW a)
    dirSE a
      | (dotS a) > (dotE a) && (dotS a) > (dotSE a) = (DirectionS, dotS a)
      | (dotE a) > (dotS a) && (dotE a) > (dotSE a) = (DirectionE, dotE a)
      | otherwise = (DirectionSE, dotSE a)

-- geometry contains point meaning the center of mass, and
-- distance north, east, south, and west to rim from center
type ObjectGeometry = ((Float,Float),(Float,Float,Float,Float))

blockCenter :: ImageBlock a -> ImagePoint
blockCenter (ImageBlock bn be bs bw _) =
  ((bw+be)/2,(bn+bs)/2)

--isNotBackgroundColorBlock :: ImageBlock StatColor -> Bool
--isNotBackgroundColorBlock = not . isBackground . blockColorPair

-- fold block center coordinates towards the sum of all x and y coordinates
-- also update the total amount of blocks considered for calculating average
foldBlockCenter :: ImageBlock StatColor -> (Float,Float,Float) -> (Float,Float,Float)
foldBlockCenter b (x,y,n)
  | (isBackground (blockColorPair b) (blockMean statColor b)) = (x,y,n)
  | otherwise = (x+bx,y+by,n+1)
  where (bx,by) = blockCenter b

foldObjectCenter :: Int -> ImageBlock ObjectInfo -> (ImagePoint, Float) ->
    (ImagePoint, Float)
foldObjectCenter i b s@((x,y),n)
  | (objectid $ value b) == i = ((x+bx,y+by),n+1)
  | otherwise = s
  where (bx,by) = blockCenter b

-- fold block center coordinates towards the sum of all x and y coordinates
-- also update the total amount of blocks for calculating average
-- used for determining the center of mass (average x and y coordinate)
-- block
-- function to decide if block belongs to object
-- sum of point coordinates and number of blocks so far
foldObjectCenterWith :: ImageBlock a -> (ImageBlock a -> Bool) ->
    (ImagePoint, Float) -> (ImagePoint, Float)
foldObjectCenterWith b isObject s@((x,y),n)
  | isObject b = ((x+bx,y+by),n+1)
  | otherwise  = s
  where (bx,by) = blockCenter b

-- if elem is found, produce a triple with prefix, Just elem, and suffix.
-- if elem is not found, produce a triple with original list, Nothing, and
-- empty list.
--liftElem :: (a -> Bool) -> [a] -> ([a], (Maybe a), [a])
--liftElem isElem s
--  | isNothing i = (s, Nothing, [])
--  | otherwise   = ((take (n-1) s), Just (s !! n), (drop n s))
--  where
--    i = findIndex isElem s
--    n = fromJust i

isDirection :: DirectionDistance -> DirectionDistance -> Bool
isDirection (a,_) (b,_) = a == b

maxDirection :: DirectionDistance -> DirectionDistance -> DirectionDistance
maxDirection a@(dir_a, dist_a) b@(dir_b, dist_b)
  | dir_b /= dir_a  = b
  | dist_b > dist_a = b
  | otherwise       = a

sumColor :: ColorName -> (ColorName, Float) -> (ColorName, Float)
sumColor c a@(ca, na)
  | c /= ca = a
  | otherwise = (ca, na+1)

sumDistShape :: ObjectInfo ->
  (Direction, Float, (Float,Float), (Float,Float)) ->
  (Direction, Float, (Float,Float), (Float,Float))
sumDistShape (ObjectInfo _ _ s dir dist) o@(d, m, (ne,ns), (fe,fs))
  | dir /= d = o
  | dist <= (m/2) && s == Edge   = (d, m, (ne+1, ns),   (fe,   fs))
  | dist <= (m/2) && s == NoEdge = (d, m, (ne,   ns+1), (fe,   fs))
  | dist >  (m/2) && s == Edge   = (d, m, (ne,   ns),   (fe+1, fs))
  | dist >  (m/2) && s == NoEdge = (d, m, (ne,   ns),   (fe,   fs+1))

foldPointDirection :: [DirectionDistance] -> DirectionDistance ->
    [DirectionDistance]
foldPointDirection ds d = map (maxDirection d) ds

foldBlockColor :: [(ColorName, Float)] -> ImageBlock ObjectInfo ->
  [(ColorName, Float)]
foldBlockColor cs b = map (sumColor $ blockColor $ value b) cs

foldBlockDistShape822 :: [(Direction, Float, (Float,Float), (Float,Float))] ->
  ImageBlock ObjectInfo -> [(Direction, Float, (Float,Float), (Float,Float))]
foldBlockDistShape822 ds b = map (sumDistShape $ value b) ds

-- block
-- function to decide if block belongs to object
-- center point relative to which direction and distance is determined
-- list of directions and max distances against which this block is folded
--foldObjectMaxDistance4 :: ImageBlock a -> (ImageBlock a -> Bool) ->
--    ImagePoint -> [DirectionDistance] -> [DirectionDistance]
--foldObjectMaxDistance4 b isObject c d
--  | isObject b = foldPointDirection d pd
--  | otherwise  = d
--  where
--        bc = blockCenter b
--        pd = pointDirection4 c bc

-- block
-- function to decide if block belongs to object
-- center point relative to which direction and distance is determined
-- list of directions and max distances against which this block is folded
-- need to consider block type - with ObjectInfo this is redundant, with
-- generic block must calculate the dirdist and need the centerpoint
--foldObjectMaxDistanceWith :: ImageBlock a -> (ImageBlock a -> Bool) ->
--    [DirectionDistance] -> [DirectionDistance]
--foldObjectMaxDistanceWith b isObject d
--  | isObject b = foldPointDirection d $ blockDirectionDistance b
--  | otherwise  = d

blockDirectionDistance :: ImageBlock ObjectInfo -> DirectionDistance
blockDirectionDistance b = (blockDir $ value b, blockDist $ value b)

foldObjectMaxDistance :: Int -> [DirectionDistance] ->
    ImageBlock ObjectInfo -> [DirectionDistance]
foldObjectMaxDistance i d b
  | (objectid $ value b) == i = foldPointDirection d $ blockDirectionDistance b
  | otherwise = d

foldObjectColor :: Int -> [(ColorName, Float)] -> ImageBlock ObjectInfo ->
  [(ColorName, Float)]
foldObjectColor i c b
  | (objectid $ value b) == i = foldBlockColor c b
  | otherwise = c

foldObjectDistShape822 :: Int ->
  [(Direction, Float, (Float,Float), (Float,Float))] ->
  ImageBlock ObjectInfo -> [(Direction, Float, (Float,Float), (Float,Float))]
foldObjectDistShape822 i d b
  | (objectid $ value b) == i = foldBlockDistShape822 d b
  | otherwise = d

-- fold block center coordinates towards the maximum and minimum values that
-- form the bounding box of the object
foldBlockMaxDistance :: ImageBlock StatColor -> (Float,Float,Float,Float) -> (Float,Float,Float,Float)
foldBlockMaxDistance b@(ImageBlock bn be bs bw _) (mn,me,ms,mw)
  | (isBackground (blockColorPair b) (blockMean statColor b)) = (mn,me,ms,mw)
  | otherwise = ((min mn bn),(max me be),(max ms bs),(min mw bw))

-- calculate the center of mass of the object contained by this forest
forestCenter :: ImageForest StatColor -> (Float,Float)
forestCenter f = ((sx / n), (sy / n))
  where
        (sx,sy,n) = foldr (foldBlockCenter . block) (0,0,0) (trees f)

-- calculate the bounding box of the object contained by this forest
forestMaxDistance :: (Float,Float) -> ImageForest StatColor -> (Float,Float,Float,Float)
forestMaxDistance (cx,cy) f =
  foldr (foldBlockMaxDistance . block) (cy,cx,cy,cx) (trees f)

-- the geometry is used for determining the direction where blocks lie related
-- to the centerpoint of the forest, and whether they are in the center region
-- or at the rim
forestGeometry :: ImageForest StatColor -> ObjectGeometry
forestGeometry f = (c, forestMaxDistance c f)
  where
        c = forestCenter f

data EdgeType =
  NoEdge |
  Edge |
  HEdge |
  VEdge |
  FEdge |
  BEdge
  deriving (Eq, Show)

data ColorName =
  ColorUnknown |
  ColorBlack |
  ColorWhite |
  ColorRed |
  ColorGreen |
  ColorBlue |
  ColorYellow |
  ColorBrown |
  ColorDarkGreen
  deriving (Eq, Show)

type ColorPair = (Int,Int)

data ObjectColor =
  ObjectColor
  { name :: ColorName
  , pair :: ColorPair
  , intensity :: Int
  , colorTolerance :: Int
  , intensityTolerance :: Int }

roadSignColors =
  [ (ObjectColor ColorRed       (128,240) 128  24 128)
  , (ObjectColor ColorYellow    ( 24,192) 128  24 128)
  , (ObjectColor ColorGreen     (128, 80) 128  24 128)
  , (ObjectColor ColorDarkGreen ( 64, 48) 128  24 128)
  , (ObjectColor ColorBlue      (192, 48) 128  24 128)
  , (ObjectColor ColorBrown     (104,184) 128  24 128)
  , (ObjectColor ColorBlack     (128,128)   0  24  64)
  , (ObjectColor ColorWhite     (128,128) 255  24  64)
  , (ObjectColor ColorUnknown   (128,128) 128 128 128)
  ]

roadSignColorNames =
  [ (ColorRed,       0.0)
  , (ColorYellow,    0.0)
  , (ColorGreen,     0.0)
  , (ColorDarkGreen, 0.0)
  , (ColorBlue,      0.0)
  , (ColorBrown,     0.0)
  , (ColorBlack,     0.0)
  , (ColorWhite,     0.0)
  , (ColorUnknown,   0.0)
  ]

isColor :: ColorPair -> Int -> ObjectColor -> Maybe ColorName
isColor cp ci (ObjectColor n p i ct it)
  | cdist < ct && idist < it = Just n
  | otherwise                = Nothing
  where
        cdist = round $ colorDistance p cp
        idist = abs $ i - ci

firstColorName :: [Maybe ColorName] -> ColorName
firstColorName cs = head $ catMaybes cs

colorPairToColorName :: [ObjectColor] -> ColorPair -> Int -> ColorName
colorPairToColorName cs cp i =
  head $ (mapMaybe (isColor cp i) cs) ++ [ColorUnknown]

data ObjectInfo =
  ObjectInfo
  { objectid :: Int
  , blockColor :: ColorName
  , blockShape :: EdgeType
  , blockDir :: Direction
  , blockDist :: Float }

type GeometryFeature = (Geometry, [Double])

data ImageObject d =
  ImageObject
  { identifier :: Int
  , forest :: ImageForest ObjectInfo
  , descriptor :: d }

analyzeTree :: ImageTree StatColor -> ImageTree ObjectInfo
analyzeTree EmptyTree = EmptyTree
analyzeTree (ImageTree p b@(ImageBlock bn be bs bw _) tnw tne tsw tse) =
  (ImageTree p
    (ImageBlock bn be bs bw (ObjectInfo o c s DirectionUnknown 0.0))
    (analyzeTree tnw) (analyzeTree tne) (analyzeTree tsw) (analyzeTree tse))
  where
        i = blockMean statColor b
        d = blockDev statColor b
        cp = blockColorPair b
        o = if (isBackground cp i) then 0 else 1
        c = colorPairToColorName roadSignColors cp i
        s = if d < 10 then NoEdge else Edge

analyzeForest :: ImageForest StatColor -> ImageForest ObjectInfo
analyzeForest (ImageForest p i r c ts) =
  (ImageForest p i r c (mapDeep analyzeTree ts))

analyzeTreeDir :: CenterPoint -> ImageTree ObjectInfo -> ImageTree ObjectInfo
analyzeTreeDir c EmptyTree = EmptyTree
analyzeTreeDir cp
    (ImageTree p
               b@(ImageBlock bn be bs bw (ObjectInfo o c s _ _))
               tnw tne tsw tse) =
  (ImageTree p
    (ImageBlock bn be bs bw (ObjectInfo o c s dir dist))
    (analyzeTreeDir cp tnw) (analyzeTreeDir cp tne)
    (analyzeTreeDir cp tsw) (analyzeTreeDir cp tse))
    where
          bc = blockCenter b
          (dir, dist) = pointDirection8 cp bc

analyzeForestDir :: CenterPoint -> ImageForest ObjectInfo ->
    ImageForest ObjectInfo
analyzeForestDir cp (ImageForest p i r c ts) =
  (ImageForest p i r c (mapDeep (analyzeTreeDir cp) ts))

objectCenter :: ImageForest ObjectInfo -> ImageObject CenterPoint
objectCenter NullForest = (ImageObject 0 NullForest (0,0))
objectCenter f = (ImageObject 1 f (sx / n, sy / n))
  where ((sx,sy),n) = foldr ((foldObjectCenter 1) . block) ((0,0),0) (trees f)

objectGeometry :: ImageObject CenterPoint -> ImageObject Geometry
objectGeometry (ImageObject i NullForest cp) =
  (ImageObject i NullForest (cp, geometry8))
objectGeometry (ImageObject i f cp) =
  (ImageObject i fd g)
  where
    fd = analyzeForestDir cp f
    d = foldl (foldObjectMaxDistance i) geometry8 (map block $ trees fd)
    g = (cp, d)

normalizeColor :: [(ColorName, Float)] -> [Double]
normalizeColor cs = map (float2Double . (/ m) . snd) $ cs
  where m = maximum $ map snd cs

createDistShape22 :: DirectionDistance -> (Direction, Float, (Float,Float), (Float,Float))
createDistShape22 (dir, dist) = (dir, dist, (0,0), (0,0))

normalizeDistShape822 :: (Direction, Float, (Float,Float), (Float,Float)) -> [Double]
normalizeDistShape822 (_,_,(ne,ns),(fe,fs)) =
  [float2Double $ ne/(ne+ns), float2Double $ fe/(fe+fs)]

normalizeDistShape822List :: [(Direction, Float, (Float,Float), (Float,Float))] -> [Double]
normalizeDistShape822List ds = concatMap normalizeDistShape822 ds

normalizeDistance :: [DirectionDistance] -> [Double]
normalizeDistance ds = map (float2Double . (/ m) . snd) ds
  where m = maximum $ map snd ds

objectFeature :: ImageObject Geometry -> ImageObject GeometryFeature
objectFeature (ImageObject i f (c, g)) =
  (ImageObject i f ((c, g), concat $
    [ normalizeColor $ init cs,
      normalizeDistance g,
      normalizeDistShape822List $ ds ] ))
  where
      bs = map block $ trees f
      cs = foldl (foldObjectColor i) roadSignColorNames bs
      ds = foldl (foldObjectDistShape822 i) (map createDistShape22 g) bs

treeToFeatureVector :: ObjectGeometry -> ImageTree StatColor -> [Double]
treeToFeatureVector g t@(ImageTree p b tnw tne tsw tse) =
  [ isRed cp
  , isYellow cp
  , isGreen cp
  , isDarkGreen cp
  , isBlue cp
  , isBrown cp
  , isBlack cp m
  , isWhite cp m
  -- edges on rim
  , (min (isTopLeftRim g bc) (isHorizontalEdge d))
  , (min (isTopLeftRim g bc) (isVerticalEdge d))
  , (min (isTopRightRim g bc) (isHorizontalEdge d))
  , (min (isTopRightRim g bc) (isVerticalEdge d))
  , (min (isBottomLeftRim g bc) (isHorizontalEdge d))
  , (min (isBottomLeftRim g bc) (isVerticalEdge d))
  , (min (isBottomRightRim g bc) (isHorizontalEdge d))
  , (min (isBottomRightRim g bc) (isVerticalEdge d))
  -- edges at center
  , (min (isTopLeftCenter g bc) (isHorizontalEdge d))
  , (min (isTopLeftCenter g bc) (isVerticalEdge d))
  , (min (isTopRightCenter g bc) (isHorizontalEdge d))
  , (min (isTopRightCenter g bc) (isVerticalEdge d))
  , (min (isBottomLeftCenter g bc) (isHorizontalEdge d))
  , (min (isBottomLeftCenter g bc) (isVerticalEdge d))
  , (min (isBottomRightCenter g bc) (isHorizontalEdge d))
  , (min (isBottomRightCenter g bc) (isVerticalEdge d))
  ]
  where
        m = blockMean statColor b
        d = treeDir statColor t
        cp = blockColorPair b
        -- cp = (fromIntegral c1, fromIntegral c2)
        bc = blockCenter b

partitionFeatureVector :: [Double] -> ([Double], [Double], [Double])
partitionFeatureVector fs = (as,bs,cs)
  where (as,ts) = splitAt 8 fs
        (bs,cs) = splitAt 8 ts

combineFeatureVector :: ([Double], [Double], [Double]) -> [Double]
combineFeatureVector (as,bs,cs) = as ++ bs ++ cs

partialMinimum :: ([Double], [Double], [Double]) -> (Double, Double, Double)
partialMinimum (as,bs,cs) = (minimum as, minimum bs, minimum cs)

partialMaximum :: ([Double], [Double], [Double]) -> (Double, Double, Double)
partialMaximum (as,bs,cs) = (maximum as, maximum bs, maximum cs)

partialNormalize :: [[Double]] -> [Double]
partialNormalize fs = (map ((* sa) . (+ (-ta))) as) ++
                      (map ((* sb) . (+ (-tb))) bs) ++
                      (map ((* sc) . (+ (-tc))) cs)
  where
        sfs = foldl (zipWith (+)) (repeat 0) fs
        (as,bs,cs) = partitionFeatureVector sfs
        (ta,tb,tc) = partialMinimum (as,bs,cs)
        (ma,mb,mc) = partialMaximum (as,bs,cs)
        (sa,sb,sc) = (1.0/(ma-ta), 1.0/(mb-tb), 1.0/(mc-tc))

treeToBlockWithFeatureVector :: ObjectGeometry -> ImageTree StatColor -> ImageBlock [Double]
treeToBlockWithFeatureVector g t@ImageTree{ block = (ImageBlock bn be bs bw _)} =
  (ImageBlock bn be bs bw (treeToFeatureVector g t))

forestToBlocksWithFeatureVectors :: ImageForest StatColor -> [ImageBlock [Double]]
forestToBlocksWithFeatureVectors f = mapDeep (treeToBlockWithFeatureVector g) (trees f)
  where g = forestGeometry f

forestToFeatureVectors :: ImageForest StatColor -> [[Double]]
forestToFeatureVectors f = mapDeep (treeToFeatureVector g) (trees f)
  where g = forestGeometry f

normalize :: [[Double]] -> [Double]
normalize fs = map ((* scaleValue) . (+ transValue)) sfs
  where
        sfs = foldl (zipWith (+)) (repeat 0) fs
        transValue = minimum sfs
        scaleValue = 1.0 / ((maximum sfs) - (minimum sfs))

cvColorImageToFeatureVector :: (Image RGB D8) -> [Double]
cvColorImageToFeatureVector i = unsafePerformIO $
  withForestFromColorCVImage i 8 8 $
      snd . descriptor . objectFeature . objectGeometry . objectCenter . analyzeForest . updateForest
--    partialNormalize . forestToFeatureVectors . divideForest . updateForest

fileToFeatureVector :: String -> [Double]
fileToFeatureVector s =
  unsafePerformIO $ do
    i <- readFromFile s
    return $ cvColorImageToFeatureVector i
