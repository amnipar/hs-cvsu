{-# LANGUAGE TypeFamilies #-}
module CVSU.Types
( ImageBlockType(..)
, cImageBlockType
, hImageBlockType
, Statistics(..)
, Stat(..)
, Dir(..)
, StatDir(..)
, StatColor(..)
, DirColor(..)
, StatDirColor(..)
, Mean(..)
, DivideMean(..)
, ColorMean(..)
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
) where

import CVSU.Bindings.Types

import Foreign.Ptr
import Foreign.Storable
import Control.DeepSeq

data ImageBlockType =
  BlockEmpty |
  BlockStatGrey |
  BlockStatColor
  deriving (Eq, Show)

cImageBlockType :: ImageBlockType -> C'image_block_type
cImageBlockType t
  | t == BlockStatGrey  = c'b_STAT_GREY
  | t == BlockStatColor = c'b_STAT_COLOR
  | otherwise           = c'b_NONE

hImageBlockType :: C'image_block_type -> ImageBlockType
hImageBlockType t
  | t == c'b_STAT_GREY  = BlockStatGrey
  | t == c'b_STAT_COLOR = BlockStatColor
  | otherwise           = BlockEmpty

data Statistics =
  Statistics
  { mean :: Double
  , variance :: Double
  } deriving Eq

newtype Stat = Stat(Int, Int) deriving (Eq, Show)
newtype Dir = Dir(Int, Int) deriving (Eq, Show)
newtype StatDir = StatDir(Stat, Dir) deriving (Eq, Show)
newtype StatColor = StatColor(Stat, Stat, Stat) deriving (Eq, Show)
newtype DirColor = DirColor(Dir, Dir, Dir) deriving (Eq, Show)
newtype StatDirColor = StatDirColor(StatDir, StatDir, StatDir) deriving (Eq, Show)

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
