module CVSU.Moments
( Moments(..)
, calculateMoments
, quadTreeMoments
) where

import CVSU.QuadForest

data Moments =
  Moments
  { momentCenter :: (Float,Float)
  , momentArea :: Float
  , momentOrientation :: Float
  , momentEccentricity :: Float
  }

calculateMoments :: [(Int,Int)] -> Moments
calculateMoments cs = Moments (cx,cy) m00 t e
  where
    m00 :: Float
    m00 = fromIntegral $ length cs
    m10 :: Float
    m10 = fromIntegral $ sum $ map fst cs
    m01 :: Float
    m01 = fromIntegral $ sum $ map snd cs
    m20 :: Float
    m20 = sum $ map ((**2).fromIntegral.fst) cs
    m02 :: Float
    m02 = sum $ map ((**2).fromIntegral.snd) cs
    m11 :: Float
    m11 = fromIntegral $ sum $ map (\(f,s) -> f*s) cs
    cx :: Float
    cx = m10 / m00
    cy :: Float
    cy = m01 / m00
    c20 :: Float
    c20 = m20 / m00 - cx**2
    c02 :: Float
    c02 = m02 / m00 - cy**2
    c11 :: Float
    c11 = m11 / m00 - cx*cy
    l1 :: Float
    l1 = 0.5 * (c20 - c02) + 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
    l2 :: Float
    l2 = 0.5 * (c20 - c02) - 0.5 * (sqrt $ 4 * c11**2 + (c20 - c02)**2)
    e :: Float
    e = sqrt $ 1 - l2 / l1
    t :: Float
    t = 0.5 * (atan $ 2 * c11 / (c20 - c02))

quadTreePixels t = [(x,y) | x <- [tx..tx+ts-1], y <- [ty..ty+ts-1]]
  where
    tx = quadTreeX t
    ty = quadTreeY t
    ts = quadTreeSize t

quadTreeMoments :: [QuadTree] -> Moments
quadTreeMoments ts = calculateMoments $ concatMap quadTreePixels ts
