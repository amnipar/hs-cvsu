module CVSU.GeometricFeatures
( colorPairToUV
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
