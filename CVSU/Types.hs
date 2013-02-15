module CVSU.Types
( cBool
, hBool
, Statistics(..)
, hStatistics
, cStatistics
, Direction(..)
, hDirection
, cDirection
, lineFromListItem
) where

import CVSU.Bindings.Types
import CVSU.Bindings.List

import Foreign.Ptr
import Foreign.Storable
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

lineFromListItem :: Ptr C'list_item -> IO ((Int,Int),(Int,Int))
lineFromListItem pitem = do
  item <- peek pitem
  line <- peek $ castPtr $ c'list_item'data item
  return $ hLine line
