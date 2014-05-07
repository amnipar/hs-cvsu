module CVSU.Types
( cBool
, hBool
, Statistics(..)
, hStatistics
, cStatistics
, statisticsNull
, RawMoments(..)
, hRawMoments
, cRawMoments
, rawMomentsNull
, Direction(..)
, hDirection
, cDirection
, hPoint
, hLine
, hWeightedLine
, lineFromListItem
, weightedLineFromListItem
) where

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import CVSU.Bindings.List

import CVSU.TypedPointer

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Concurrent
import Control.DeepSeq

cBool :: Bool -> C'truth_value
cBool True = (fromIntegral 1)
cBool False = (fromIntegral 0)

hBool :: C'truth_value -> Bool
hBool = (/=0) . fromIntegral

data Statistics =
  Statistics
  { items :: Double
  , sum1 :: Double
  , sum2 :: Double
  , mean :: Double
  , variance :: Double
  , deviation :: Double
  } deriving (Eq, Show)

hStatistics :: C'statistics -> Statistics
hStatistics (C'statistics n s1 s2 m v d) =
  Statistics
    (realToFrac n)
    (realToFrac s1)
    (realToFrac s2)
    (realToFrac m)
    (realToFrac v)
    (realToFrac d)

cStatistics :: Statistics -> C'statistics
cStatistics (Statistics n s1 s2 m v d) =
  C'statistics
    (realToFrac n)
    (realToFrac s1)
    (realToFrac s2)
    (realToFrac m)
    (realToFrac v)
    (realToFrac d)

statisticsAlloc :: IO (ForeignPtr C'statistics)
statisticsAlloc = do
  ptr <- c'statistics_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'statistics_free ptr)
    else error "Memory allocation failed in statisticsAlloc"

statisticsNull = Statistics 0 0 0 0 0 0

statisticsFromPtr :: Ptr C'statistics -> IO (Statistics)
statisticsFromPtr pstat = do
  stat <- peek pstat
  return $ hStatistics stat

instance Pointable Statistics where
  pointableType _ = PStatistics
  pointableNull = statisticsNull
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_statistics =
      statisticsFromPtr (castPtr v)
    | otherwise =
      error $ "Unable to convert " ++ (showPointableType l) ++ " to Statistics"
  pointableInto stat = do
    fstat <- statisticsAlloc
    withForeignPtr fstat $ \pstat -> do
      poke pstat (cStatistics stat)
      typedPointerCreate c't_statistics 1 0 (castPtr pstat)

data RawMoments =
  RawMoments
  { m00Raw :: Double
  , m10Raw :: Double
  , m01Raw :: Double
  , m11Raw :: Double
  , m20Raw :: Double
  , m02Raw :: Double
  }

hRawMoments :: C'raw_moments -> RawMoments
hRawMoments (C'raw_moments m00 m10 m01 m11 m20 m02) =
  RawMoments
    (realToFrac m00)
    (realToFrac m10)
    (realToFrac m01)
    (realToFrac m11)
    (realToFrac m20)
    (realToFrac m02)

cRawMoments :: RawMoments -> C'raw_moments
cRawMoments (RawMoments m00 m10 m01 m11 m20 m02) =
  C'raw_moments
    (realToFrac m00)
    (realToFrac m10)
    (realToFrac m01)
    (realToFrac m11)
    (realToFrac m20)
    (realToFrac m02)

rawMomentsAlloc :: IO (ForeignPtr C'raw_moments)
rawMomentsAlloc = do
  ptr <- c'raw_moments_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'raw_moments_free ptr)
    else error "Memory allocation failed in rawMomentsAlloc"

rawMomentsNull = RawMoments 0 0 0 0 0 0

rawMomentsFromPtr praw = do
  raw <- peek praw
  return $ hRawMoments raw

instance Pointable RawMoments where
  pointableType _ = PRawMoments
  pointableNull = rawMomentsNull
  pointableFrom (C'typed_pointer l c t v)
    | l == c't_raw_moments =
      rawMomentsFromPtr (castPtr v)
    | otherwise =
      error $ "Unable to convert " ++ (showPointableType l) ++ " to RawMoments"
  pointableInto raw = do
    fraw <- rawMomentsAlloc
    withForeignPtr fraw $ \praw -> do
      poke praw (cRawMoments raw)
      typedPointerCreate c't_raw_moments 1 0 (castPtr praw)

data Direction =
  DirN  |
  DirNE |
  DirE  |
  DirSE |
  DirS  |
  DirSW |
  DirW  |
  DirNW |
  DirH  |
  DirV  |
  DirR  |
  DirF  |
  DirN4 |
  DirN8 deriving (Eq, Show)

cDirection :: Direction -> C'direction
cDirection d
  | d == DirN  = c'd_N
  | d == DirNE = c'd_NE
  | d == DirE  = c'd_E
  | d == DirSE = c'd_SE
  | d == DirS  = c'd_S
  | d == DirSW = c'd_SW
  | d == DirW  = c'd_W
  | d == DirNW = c'd_NW
  | d == DirH  = c'd_H
  | d == DirV  = c'd_V
  | d == DirR  = c'd_R
  | d == DirF  = c'd_F
  | d == DirN4 = c'd_N4
  | d == DirN8 = c'd_N8

hDirection :: C'direction -> Direction
hDirection d
  | d == c'd_N  = DirN
  | d == c'd_NE = DirNE
  | d == c'd_E  = DirE
  | d == c'd_SE = DirSE
  | d == c'd_S  = DirS
  | d == c'd_SW = DirSW
  | d == c'd_W  = DirW
  | d == c'd_NW = DirNW
  | d == c'd_H  = DirH
  | d == c'd_V  = DirV
  | d == c'd_R  = DirR
  | d == c'd_F  = DirF
  | d == c'd_N4 = DirN4
  | d == c'd_N8 = DirN8
  | otherwise = error "unspecified direction"

hPoint :: C'point -> (Int,Int)
hPoint (C'point x y) = (fromIntegral x, fromIntegral y)

hLine :: C'line -> ((Int,Int),(Int,Int))
hLine (C'line start end) = (hPoint start, hPoint end)

hWeightedLine :: C'weighted_line -> (((Int,Int),(Int,Int)),Double)
hWeightedLine (C'weighted_line start end weight) =
  ((hPoint start, hPoint end), realToFrac weight)

lineFromListItem :: Ptr C'list_item -> IO ((Int,Int),(Int,Int))
lineFromListItem pitem = do
  item <- peek pitem
  line <- peek $ castPtr $ c'list_item'data item
  return $ hLine line

weightedLineFromListItem :: Ptr C'list_item -> IO (((Int,Int),(Int,Int)),Double)
weightedLineFromListItem pitem = do
  item <- peek pitem
  line <- peek $ castPtr $ c'list_item'data item
  return $ hWeightedLine line
