module CVSU.Types
( Stat(..)
, Dir(..)
, StatDir(..)
, StatColor(..)
, DirColor(..)
, StatDirColor(..)
, Mean(..)
, DivideMean(..)
, Directed(..)
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
) where


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
