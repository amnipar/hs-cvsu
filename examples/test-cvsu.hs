{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage as CVSU
import CVSU.Integral
import CVSU.ImageTree as T
import CVSU.Edges as E
import CV.Image
import CV.Pixelwise
import CV.ColourUtils
import CV.Drawing
import CV.ImageOp
import Utils.Rectangle
import Data.Function
import Data.List
import Data.Ord
import Control.Monad
import Control.Applicative
import ReadArgs

import System.IO.Unsafe
import GHC.Float
import Debug.Trace

import Data.Number.Erf

-- scan lines, compare to each line on the previous col
-- if line is attached to another line, add an equivalence relation
-- lines are stored in coordinate order, so could add some skipping logic?
-- next, scan again and construct boxes by equivalence
-- box contains a list of component id's; for each line, check if it is equivalent with one of them
-- if it is, add to box, extending box dimensions

-- initialize boxes from first col, assign component id's
-- two box lists: 'finished' and 'unfinished', first one starts empty
-- box has dimensions and last line that acts as 'interface' to that box
-- scan lines, compare to each unfinished box, add to it

columnwiseBoxing :: Int -> [[(Int,Int,Int)]] -> [(Int,Int,Int,Int)]
columnwiseBoxing s cs = (filter isGoodBox rt) ++ rb
  where
    (rb,rt) = foldr colscan ([],map initbox $ last cs) $ init cs
    initbox (x,y1,y2) = (x,y1,x,y2)
    isGoodBox (x1,y1,x2,y2) = (x2-x1 > s || y2-y1 > s) && x2-x1 < 36*s && y2 > 5*s && y1 < 30*s
    colscan c (bs,ts) = (bs',nts)
      where
        nts = ts' ++ (filter ((flip notElem) ms) ns)
        ((bs',ts'),(ns,ms)) = foldr growboxes ((bs,[]),(ts,[])) c
    growboxes (x,y1,y2) ((bs,ts),(ns,ms))
      | found == False = ((bs',(x,y1,x,y2):ts'),(ns',ms'))
      | otherwise      = ((bs',ts'),(ns',ms'))
      where
        (((bs',ts'),(ns',ms')),(_,_,_,found)) = foldr scantemp (((bs,ts),(ns,ms)),(x,y1,y2,False)) ns
    scantemp b@(bx1,by1,bx2,by2) (((bs,ts),(ns,ms)),(x,y1,y2,f))
      | bx1-s > x && isGoodBox b = (((b:bs,ts),(ns,b:ms)),(x,y1,y2,f))
      | bx1-s == x && by1 <= y2 && by2 >= y1 = (((bs,addBox ts),(ns,b:ms)),(x,y1,y2,True))
      | otherwise = (((bs,ts),(ns,ms)),(x,y1,y2,f))
      where
        addBox ts
          | bx2-x < 36*s && (null $ filter (isWithin (x,y1,y2)) ts) = (x,(min by1 y1),bx2,(max by2 y2)):ts
          | otherwise = ts
        isWithin (x,y1,y2) (bx1,by1,bx2,by2) = x == bx1 && y1 >= by1 && y2 <= by2

data Stripe =
  Stripe
  { sid :: !Int
  , coords :: !(Int,Int,Int)
  }

-- read cols one by one
-- assign id (carry previous id in a parameter)
-- compare with all stripes of previous col
-- if equal, add equivalence to relation list
-- need to know:
--  *last id
--  *stripes in previous col
--  *next stripe
stripeEquivalences :: [[(Int,Int,Int)]] -> ([(Int,Int)],[Stripe])
stripeEquivalences cs = (finalEqs,finalStripes)
  where
    (initId,initEq,initStripes) = foldl addStripe (0,[],[]) (head cs)
    (_,finalEqs,finalStripes,_) = foldl handleCol (initId,initEq,[],initStripes) (tail cs)
    addStripe (lastId,eqs,stripes) stripe = newId `seq` (newId,eqs,((Stripe newId stripe):stripes))
      where
        newId = lastId + 1
    addEquivalence eqs ((Stripe aid a),(Stripe bid b))
      | isEquivalent a b = ((aid,bid):eqs)
      | otherwise = eqs
    isEquivalent (_,ay1,ay2) (_,by1,by2) = (by1 <= ay2) && (by2 >= ay1)
    handleCol (lastId,eqs,stripes,lastStripes) col =
      (newLastId,eqs++newEqs,stripes++(sortBy (comparing sid) lastStripes),newStripes)
      where
        (newLastId,_,newStripes) = foldl addStripe (lastId,eqs,[]) col
        newEqs = foldl addEquivalence [] pairs
        pairs = [(a,b) | a <- lastStripes, b <- newStripes]

-- check stripes one by one
-- compare id to list of id's in each box
-- if id matches, add to list of id's, add to list of stripes, increase bounding box
-- when first match is found, search should be terminated -> make own recursion
-- add new box always to beginning of list, so search terminates sooner
-- need to carry around the previous list and remaining list and then concatenate?
equivalenceComponents :: Int -> ([(Int,Int)],[Stripe]) -> [((Int,Int,Int,Int),([Int],[(Int,Int,Int)]))]
equivalenceComponents d (eqs,stripes) = completed ++ active
  where
    (_,completed,active) = foldl addComponents (eqs,[],[]) stripes
    addComponents (eqs,cc,ac) s = findComponent s (eqs,cc,ac)
    sortedEqs = sortBy (comparing fst) eqs
    -- stop the recursion when the equivalence list is exhausted
    findEq [] _ _ = False
    findEq ((e1,e2):eqs) a1 a2
      -- if id's match either way, there is an equivalence
      | (a1 == e1 && a2 == e2) || (a2 == e1 && a1 == e2) = True
      -- if both comparison id's are larger, we won't find an equivalence here
      | (e1 > a1) && (e1 > a2) && (e2 > a1) && (e2 > a2) = False
      -- otherwise compare reqursion
      | otherwise = findEq eqs a1 a2
    -- return both eqs and bool.
    -- findEq should return the pruned list of eqs; too small id's are removed.
    isEq _ _ [] = False
    isEq eqs sid1 (sid2:ids)
      | findEq eqs sid1 sid2 = True
      | otherwise = isEq eqs sid1 ids
    pruneEq [] _ = []
    pruneEq eqs@((e1,e2):es) sid
      | (e1 < sid) && (e2 < sid) = pruneEq es sid
      | otherwise = eqs
    -- return list of completed and active components.
    findComponent s@(Stripe sid _) (eqs,cc,[]) = ((pruneEq eqs sid),cc,[initComponent s])
    findComponent s@(Stripe sid (sx,_,_)) (eqs,cc,(c@((_,_,cx,_),(ids,_)):ac))
      | isEq eqs sid ids = ((pruneEq eqs sid),cc,(consIfSizeOk (expandComponent c s) ac))
      | cx + d < sx = findComponent s (eqs,c:cc, ac)
      | otherwise = nextCC c
      where
        consIfSizeOk c@((x1,y1,x2,y2),_) cs
          | (x2-x1) > d*60 = cs
          | (y2-y1) > d*20 = cs
          | otherwise = (c:cs)
        nextCC c = (es,nc,c:na)
          where (es,nc,na) = findComponent s (eqs,cc,ac)
    initComponent (Stripe sid s@(sx,sy1,sy2)) = ((sx,sy1,sx+d,sy2),([sid],[s]))
    expandComponent ((x1,y1,x2,y2),(ids,stripes)) (Stripe sid s@(sx,sy1,sy2)) =
      (((min x1 sx),(min y1 sy1),(max x2 (sx+d)),(max y2 sy2)),(sid:ids,s:stripes))

equivalenceBoxes :: Int -> ([(Int,Int)],[Stripe]) -> [(Int,Int,Int,Int)]
equivalenceBoxes d (eqs,stripes) =
  filter sizeLimit $ map fst $ equivalenceComponents d (eqs,stripes)
    where
      sizeLimit (x1,y1,x2,y2) = (x2-x1) > 2*d || (y2-y1) > 3*d

-- for each stripe in previous col
-- check every stripe in current col
-- if they are equivalent, add equivalence relation

columnwiseChanges :: ImageForest Stat -> [[(Int,Int,Int)]]
columnwiseChanges f@(ImageForest ptr _ r c _ ts) = colLines
  where
    ds = map (statDev . T.value . block) ts
    avgDev = ceiling $ (fromIntegral $ sum ds) / (fromIntegral $ length ds)
    toLine f (x,y) = liftM toL $ getTree f (x,y)
      where
        toL (ImageTree _ _ (ImageBlock x y _ h (Stat(_,d))) _ _ _ _) =
          ((x, y, y+h),d)
    --toLine (ImageTree _ (ImageBlock n e s w (Stat(_,d))) _ _ _ _) = trace (show (w,n,e,s)) $
    --  ((round $ (e+w)/2, round n, round s),d)
    cols = map (unsafePerformIO . mapM (toLine f)) $! [[(x,y) | y <- [0..r-1]] | x <- [0..c-1]]
    colChanges dt (cs,(c1@(x1,ya1,yb1),d1)) c@((x,ya,yb),d)
      | d1 > dt && d > dt = (cs, ((x1,ya1,yb),d)) -- trace ("a"++show(x,ya1,yb1,ya,yb)) $
      | d1 > dt && d <= dt = (c1:cs, c) -- trace ("b"++show(x,ya1,yb1,ya,yb)) $
      | otherwise = (cs, c) -- trace ("c"++show(x,ya1,yb1,ya,yb)) $
    getCol dt (cs,(c@(x,y1,y2),d))
      | d > dt    = c:cs
      | otherwise = cs
    handleCol dt c = getCol dt $ foldl (colChanges dt) ([],head c) (tail c)
    colLines = map (handleCol avgDev) cols

columnwiseEntropy :: ImageForest Statistics -> [[(Int,Int,Int)]]
columnwiseEntropy f@(ImageForest ptr _ r c _ ts) = colLines
  where
    toLine f (x,y) = liftM toL $ getTree f (x,y)
      where
        toL t@ImageTree{block=ImageBlock{T.x=bx,T.y=by,h=bh}} =
          ((bx, by, by+bh),not $ neighborhoodEntropy t)
    cols = map (unsafePerformIO . mapM (toLine f)) $! [[(x,y) | y <- [0..r-1]] | x <- [0..c-1]]
    colChanges (cs,(c1@(x1,ya1,yb1),e1)) c@((x,ya,yb),e)
      | e1 && e = (cs, ((x1,ya1,yb),e)) -- trace ("a"++show(x,ya1,yb1,ya,yb)) $
      | e1 && (not e) = (c1:cs, c) -- trace ("b"++show(x,ya1,yb1,ya,yb)) $
      | otherwise = (cs, c) -- trace ("c"++show(x,ya1,yb1,ya,yb)) $
    getCol (cs,(c@(x,y1,y2),e))
      | e = c:cs
      | otherwise = cs
    handleCol c = getCol $ foldl colChanges ([],head c) (tail c)
    colLines = map handleCol cols

joinBoxes :: Int -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
joinBoxes s bs = combine bs
  where
    combine [] = []
    combine (b:bs)
      | isSmall b = (newBox (b:(pick b bs))) ++ (combine bs)
      | isLong b = combine bs
      | otherwise = (b:(combine bs))
    dx (x1,_,x2,_) = x2-x1
    dy (_,y1,_,y2) = y2-y1
    isTooSmall b = dx b < 5*s || dy b < 5*s
    isSmall b = dx b < 8*s || dy b < 8*s
    isLong b = dy b < 8*s && dx b > 60*s
    newBox (b:bs)
      | isTooSmall nb = []
      | isTooSmall b = [nb]
      | otherwise = [b,nb]
      where
        nb = foldl accBounds b bs
        accBounds (ax1,ay1,ax2,ay2) (bx1,by1,bx2,by2) =
          ((min ax1 bx1),(min ay1 by1),(max ax2 bx2),(max ay2 by2))
    pick _ [] = []
    pick a@(ax1,ay1,ax2,ay2) (b@(bx1,by1,bx2,by2):bs)
      | w >= 12*s && w <= 60*s && h >= 8*s && h <= 14*s = [b]
      | w > 60*s = []
      | h > 14*s = pick a bs
      | otherwise = b:(pick a bs)
      where
        w = bx2-ax1
        h = (max by2 ay2)-(min by1 ay1)

createFromPixels :: Int -> Int -> [((Int,Int),Float)] -> IO (Image GrayScale D32)
createFromPixels w h ps = do
  i <- create (w,h)
  mapM_ (sp i) ps
  return i
  where
        minV = minimum $ map snd ps
        maxV = maximum $ map snd ps
        sp i ((x,y),v) = setPixel (x,y) (v/maxV) i
        --i = imageFromFunction (w,h) (const 0)

drawChanges :: [[(Int,Int,Int)]] -> Image RGB D32 -> Image RGB D32
drawChanges ls i =
  i
  <## concat [[lineOp (1,0,0) 1 (x,y1) (x,y2) | (x,y1,y2) <- cs] | cs <- ls]

drawBoxes :: (Float,Float,Float) -> [(Int,Int,Int,Int)] -> Image RGB D32 -> Image RGB D32
drawBoxes c bs i = trace (show $ length bs) $
  i
  <## [rectOp c 1 r | r <- map toRect bs]
  where
    toRect (x1,y1,x2,y2) = mkRectangle (x1,y1) (x2-x1,y2-y1)

drawEdges :: EdgeImage -> Int -> Image RGB D32 -> Image RGB D32
drawEdges eimg s i =
  i
  <## [lineOp (0,1,1) 2 (x,y) (x+s,y) | (Edge x y _ v) <- he] -- ((abs v) / maxV)
  <## [lineOp (0,1,1) 2 (x,y) (x,y+s) | (Edge x y _ v) <- ve]
  where
        maxV = maximum $ map (abs . E.value) (he ++ ve)
        he = filter ((>1) . abs . E.value) $ concat $ hedges eimg
        ve = filter ((>1) . abs . E.value) $ concat $ vedges eimg

drawBlocks :: Image RGB D32 -> ImageForest Statistics -> Image RGB D32
drawBlocks i f =
  i
  -- <## [rectOp (0,1,1) (-1) r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  <## [rectOp (toColor m 255) (-1) r | (r,m) <- map (toRect.block) ts]
  <## [circleOp (0,1,1) (x,y) r (Stroked 1) | (x,y,r) <- map ((toCircle maxD).block) ts]
  -- <## concat [unsafePerformIO $ nlines t | t <- ts]
  where
    ts = concatMap getTrees $ trees f
    maxM = maximum $ map (mean . T.value . block) ts
    maxD = maximum $ map (deviation . T.value . block) ts

    toColor m maxM = (c,c,c) where c = double2Float $ m / maxM
    -- toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)
    toRect (ImageBlock x y w h v) = (mkRectangle (x,y) (w,h), mean v)
    toCircle maxD (ImageBlock x y w h v) =
      (x+(w`div`2), y+(h`div`2), round $ (min maxD $ deviation v) / maxD * (fromIntegral w))
    nlines t@ImageTree{block=ImageBlock{T.x=tx,T.y=ty,T.w=tw,T.h=th}} = do
      (ns::[ImageTree Statistics]) <- treeNeighbors t
      return [lineOp (1,0,0) 1 (tx+(tw`div`2),ty+(th`div`2)) (nx+(nw`div`2),ny+(nh`div`2))
        | n@ImageBlock{T.x=nx,T.y=ny,T.w=nw,T.h=nh} <- map block ns]

drawRects :: Image RGB D32 -> [ImageTree a] -> Image RGB D32
drawRects i ts =
  i
  <## [rectOp (0,1,1) 1 r | r <- map (toRect.block) ts]
  where
    toRect (ImageBlock x y w h _) = mkRectangle (x,y) (w,h)

{-
drawRegions :: (Int,Int) -> Int -> [((Int,Int),Float)] -> IO (Image GrayScale D32)
drawRegions (w,h) s rs = do
  i <- create (w*s,h*s)
  return $
    i <## [rectOp (v/maxV) (-1) (mkRectangle (x*s,y*s) (s,s)) | ((x,y),v) <- rs]
  where
    maxV = maximum $ map snd rs
-}

meanFilter :: PixelImage -> Int -> IO (Image GrayScale D32)
meanFilter pimg r = do
  int <- createIntegralImage pimg
  vs <- mapM (values int r) cs
  createFromPixels w h vs
  where
    w = width pimg
    h = height pimg
    cs = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    values int r (x,y) = do
      v <- integralMeanByRadius int r (x,y)
      return ((x,y),double2Float v)

varianceFilter :: PixelImage -> Int -> IO (Image GrayScale D32)
varianceFilter pimg r = do
  int <- createIntegralImage pimg
  vs <- mapM (values int r) cs
  createFromPixels w h vs
  where
    w = width pimg
    h = height pimg
    cs = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    values int r (x,y) = do
      v <- integralVarianceByRadius int r (x,y)
      return ((x,y),double2Float $ sqrt v)

adaptiveThreshold :: PixelImage -> Int -> IO (Image GrayScale D32)
adaptiveThreshold pimg r = do
  int <- createIntegralImage pimg
  vs <- mapM (values pimg int r) cs
  createFromPixels w h vs
  where
    w = width pimg
    h = height pimg
    cs = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    values pimg int r (x,y) = do
      v <- CVSU.getPixel pimg (x,y)
      m <- liftM double2Float $ integralMeanByRadius int r (x,y)
      return ((x,y),if v >= m then 1 else 0)

sauvolaThreshold :: PixelImage -> Int -> IO (Image GrayScale D32)
sauvolaThreshold pimg r = do
  int <- createIntegralImage pimg
  ds <- mapM (integralVarianceByRadius int r) cs
  vs <- mapM (values pimg int r (avgD ds)) cs
  createFromPixels w h vs
  where
    w = width pimg
    h = height pimg
    --maxD ds = maximum $ map (double2Float.sqrt) ds
    avgD ds = (sum ss) / (fromIntegral $ length ss)
      where ss = map (double2Float.sqrt) ds
    cs = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    values pimg int r d (x,y) = do
      v <- CVSU.getPixel pimg (x,y)
      m <- liftM double2Float $ integralMeanByRadius int r (x,y)
      s <- liftM (double2Float.sqrt) $ integralVarianceByRadius int r (x,y)
      return ((x,y),if v >= (t d m s) then 1 else 0)
      where
        t d m s = m * (1 + 0.5 * (s / d - 1))

minimumByRadius :: PixelImage -> Int -> (Int,Int) -> IO (Float)
minimumByRadius pimg r (x,y) = do
  ps <- mapM (CVSU.getPixel pimg) cs
  return $ minimum ps
  where
        w = width pimg
        h = height pimg
        cs = [(i,j) | i <- [(max (x-r) 0)..(min (w-1) (x+r))], j <- [(max (y-r) 0)..(min (h-1) (y+r))]]

fengThreshold :: PixelImage -> Int -> IO (Image GrayScale D32)
fengThreshold pimg r = do
  int <- createIntegralImage pimg
  ds <- mapM (integralVarianceByRadius int r) cs
  vs <- mapM (values pimg int r (maxD ds)) cs
  createFromPixels w h vs
  where
    w = width pimg
    h = height pimg
    maxD ds = maximum $ map (double2Float.sqrt) ds
    cs = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
    values pimg int r d (x,y) = do
      v <- CVSU.getPixel pimg (x,y)
      minV <- minimumByRadius pimg r (x,y)
      m <- liftM double2Float $ integralMeanByRadius int r (x,y)
      s <- liftM (double2Float.sqrt) $ integralVarianceByRadius int r (x,y)
      locS <- liftM (double2Float.sqrt) $ integralVarianceByRadius int (3*r) (x,y)
      return ((x,y),if v >= (t minV locS m s) then 1 else 0)
      where
        t minV locS m s = (1 - a1) * m + a2 * as * (m - minV) + a3 * minV
          where
            as = s/locS
            g = 2
            a1 = 0.12
            k1 = 0.25
            k2 = 0.04
            a2 = k1 * as**g
            a3 = k2 * as**g
{-
meanRegions :: PixelImage -> Int -> IO (Image GrayScale D32)
meanRegions pimg r = do
  int <- createIntegralImage pimg
  rs <- mapM (regions int r) cs
  drawRegions (w',h') r rs
  where
    w = width pimg
    h = height pimg
    w' = w `div` r
    h' = h `div` r
    cs = [(x,y) | x <- [0..w'-1], y <- [0..h'-1]]
    regions int r (x,y) = do
      v <- integralMeanByRect int (x*r,y*r) (r,r)
      return ((x,y),double2Float v)
-}

integralBlocks :: PixelImage -> Int -> IO ([(Int,Int,Int,Int)])
integralBlocks pimg s = do
  int <- createIntegralImage pimg
  bs <- mapM (iblocks int s) cs
  return $ map fst $ filter ((>0).snd) bs
  where
        w = width pimg
        h = height pimg
        w' = w `div` s
        h' = h `div` s
        cs = [(x,y) | x <- [0..w'-1], y <- [0..h'-1]]
        iblocks int r (x,y) = do
          s1 <- integralStatisticsByRect int (x*s,y*s) (s,s)
          s2 <- integralStatisticsByRect int ((x-4)*s,(y-4)*s) (9*s,9*s)
          return ((x*s,y*s,(x+1)*s,(y+1)*s),(hasChange s1 s2))
          where
            hasChange s1 s2 = if d1 > d2 then 1 else 0
              where
                n2 = ((items s2) - (items s1))
                m2 = ((sum1 s2) - (sum1 s1)) / n2
                d1 = deviation s1
                d2 = sqrt $ ((sum2 s2) - (sum2 s1)) / n2 - m2**2

forestRegions :: Double -> ImageForest Statistics -> IO [ImageTree Statistics]
forestRegions threshold f = do
  rs <- mapM (toEqual threshold) $ sortBy (comparing (deviation . T.value . block)) $ trees f
  return $ filter ((/=0).classId) rs
  where
    testEqual :: ImageTree Statistics -> [ImageTree Statistics] -> IO (ImageTree Statistics)
    testEqual t [] = return t
    testEqual t (n:ns) = do
      if (treeDistance t n < 1)
        then do
          (t1,_) <- treeClassUnion (t,n)
          return t1
        else return t
    toEqual threshold tree = do --(trees, classes)
      ns <- treeNeighbors tree
      testEqual tree $ sortBy (comparing (treeDistance tree)) $ filter ((<threshold).treeDev) ns

colorList = concat $ repeat
  [ (0,0,1)
  , (0,1,0)
  , (1,0,0)
  , (0,1,1)
  , (1,0,1)
  , (1,1,0)
  , (0,0,0.5)
  , (0,0.5,0)
  , (0.5,0,0)
  , (0,0.5,0.5)
  , (0.5,0,0.5)
  , (0.5,0.5,0)
  ]

drawRegions :: Image RGB D32 -> [ImageTree Statistics] -> Image RGB D32
drawRegions i ts =
  i <## [rectOp (treeColor colors c) (-1)
      (mkRectangle (x,y) (w,h)) |
      ImageTree{classId=c,block=(ImageBlock x y w h _)} <- ts]
  where
    colors = regionColors ts colorList
    assignColor (ts,(c:cs)) t =
      case (lookup (classId t) ts) of
        Just rc -> (ts,(c:cs))
        Nothing -> (((classId t),c):ts,cs)
    treeColor cs cid =
      case (lookup cid cs) of
        Just c  -> c
        Nothing -> (0,0,0)
    regionColors ts cs = fst $ foldl assignColor ([],cs) ts

drawEntropy :: Image RGB D32 -> [ImageTree Statistics] -> Image RGB D32
drawEntropy i ts =
  i <## [rectOp (toColor e) (-1) (mkRectangle (x,y) (w,h)) | (x,y,w,h,e) <- bs]
  where
        bs = map toEntropy ts
        --toE (_,_,_,_,e) = e
        --es = map toE bs
        --minE = minimum es
        --maxE = maximum es
        --avgE = (sum es) / (fromIntegral $ length es)
        toColor e -- = double2Float  $ (e-minE) / (maxE-minE)
          | e == False = (1,1,1)
          | otherwise = (0,0,0)
        toEntropy t = (x,y,w,h,neighborhoodEntropy t)
          --unsafePerformIO $ do
          --cs <- treeChildStatistics t
          --let
          --    s2 = max 1 $ devDeviation cs
          --    ps = map (prob.(/s2).abs.(subtract s).deviation) cs
          --return (x,y,w,h,neighborhoodEntropy t)
          where
            (ImageBlock x y w h _) = block t
                --m = treeMean t
                --s = max 1 $ treeDev t
                --plogp p = p * logBase 2 p


treeDev = deviation . T.value . block

treeMean = mean . T.value . block

treeDistance t1 t2 = dm / sd
  where
    dm = abs $ treeMean t1 - treeMean t2
    sd = max 1 (max (treeDev t1) (treeDev t2))

devDeviation vs = sqrt var
  where
    ds = map deviation vs
    n = fromIntegral $ length ds
    sum1 = sum ds
    mean = sum1 / n
    sum2 = sum $ map (**2) ds
    var = max 0 $ sum2/n - mean**2

devDistance t1 t2 = ds / ss
  where
    s1 = treeDev t1
    s2 = treeDev t2
    ds = abs $ s1 - s2
    ss = sqrt $ max 0.00000001 $ (s1**2 + s2**2) / 2 - ((s1+s2)/2)**2

listStatistics vs = Statistics n s1 s2 m v s
  where
    n = sum $ map items vs
    s1 = sum $ map sum1 vs
    s2 = sum $ map sum2 vs
    m = s1 / n
    v = max 0 $ s2 / n - m**2
    s = sqrt v

checkConsistent :: ImageTree Statistics -> Bool
-- checkConsistent t = trace (show t) $ (<1) $ treeDev t
checkConsistent = (<1.5) . treeDev

checkEqual :: ImageTree Statistics -> ImageTree Statistics -> Bool
checkEqual t1 t2 = (treeDistance t1 t2 < 1) && ((<2) $ abs $ treeDev t1 - treeDev t2)

checkQuadrantConsistent :: ImageTree Statistics -> Bool
checkQuadrantConsistent t = unsafePerformIO $ do
  cs <- treeChildStatistics t
  let
    d2 = devDeviation cs
  return $ (maxMeanDiff cs < d) && (maxDevDiff cs < d2)
  where
    m = treeMean t
    d = treeDev t
    maxMeanDiff ts = maximum $ map (abs.(\x -> x-m).mean) ts
    maxDevDiff ts = maximum $ map (abs.(\x -> x-d).deviation) ts

sqrt2 = sqrt 2
prob z = erfc $ z / sqrt2
plogp p = p * logBase 2 p

neighborhoodEntropy :: ImageTree Statistics -> Bool
neighborhoodEntropy t
  | treeDev t < 1 = True
  | otherwise = unsafePerformIO $ do
    ns <- treeNeighbors t
    let
      nstats = map (T.value . block) (t:ns)
      lstats = listStatistics nstats
      s = max 1 $ deviation lstats
      m = mean lstats
      e = -(sum $ map (plogp.prob.(/s).abs.(subtract m).mean) nstats)
    return $ e < 1

checkEntropy :: ImageTree Statistics -> Bool
checkEntropy t
  | treeDev t < 1 = True
  | treeWidth t <= 4 = True
  | otherwise = unsafePerformIO $ do
    cs <- treeChildStatistics t
    ns <- treeNeighbors t
    let
      nstats = listStatistics  $ map (T.value . block) (t:ns)
      ds = max 1 $ deviation nstats
      m = treeMean t
      s = treeDev t
      se = -(sum $ map (plogp.prob.(/ds).abs.(subtract s).deviation) cs)
      me = -(sum $ map (plogp.prob.(/ds).abs.(subtract m).mean) cs)
    return $ (se < 1) && (me < 1)
    --where
    --  m = treeMean t
    --  s = treeDev t
    --  s1 = max 1 $ s

measureEntropy :: ImageTree Statistics -> ImageTree Statistics -> Bool
measureEntropy t1 t2 = unsafePerformIO $ do
  ns1 <- treeNeighbors t1
  ns2 <- treeNeighbors t2
  let
    nstats = listStatistics $ map (T.value . block) (ns1 ++ ns2)
    ds = max 1 $ deviation nstats
    v1 = T.value $ block t1
    v2 = T.value $ block t2
    vs = [v1,v2]
    n = items v1 + items v2
    m = (sum1 v1 + sum1 v2) / n
    s = sqrt $ max 0 $ (sum2 v1 + sum2 v2) / n - m**2
    s1 = max 1 s
    me = -(sum $ map (plogp.prob.(/s1).abs.(subtract m).mean) vs)
    se = -(sum $ map (plogp.prob.(/ds).abs.(subtract s).deviation) vs)
  return $ (se < 1) && (me < 1)

getTrees :: ImageTree a -> [ImageTree a]
getTrees EmptyTree = []
getTrees t
  | null cs = [t]
  | otherwise = cs
  where
    cs = concatMap getTrees [nw t, ne t, sw t, se t]

avgDev :: ImageForest Statistics -> Double
avgDev f = (sum ds) / (fromIntegral $ length ds)
  where ds = map (deviation . T.value . block) $ trees f

main = do
  (sourceFile, targetFile, size) <- readArgs
  img :: Image RGB D32 <- readFromFile sourceFile
  pimg <- readPixelImage sourceFile
  --mimg <- meanFilter pimg 1
  --bs <- integralBlocks pimg 8
  forest <- createForest pimg (size,size)
  withForest forest $ \f -> do
  --  let
  --    es = columnwiseChanges f
  --    bs = equivalenceBoxes 8 $ stripeEquivalences es
  --    bs2 = joinBoxes 8 $ (sortBy (comparing left)) $ (sortBy (comparing top)) bs
  --  saveImage targetFile $ drawBoxes (0,1,1) bs $ drawChanges es img -- $ drawBlocks forest
    -- rs <- forestRegions 1 f
    nf <- forestSegment 4 checkEntropy checkEqual f
      --checkConsistent checkEqual f
    --print $ show $ avgDev forest
    saveImage targetFile $ drawRegions img $ filter ((/=0).classId) $ concatMap getTrees $ trees nf
    saveImage "blocks.png" $ drawBlocks img nf
    saveImage "rects.png" $ drawRects img $ concatMap getTrees $ trees nf
    saveImage "entropy.png" $ drawEntropy img $ trees f

  --saveImage targetFile $ drawBoxes (0,1,1) bs img
  --mimg <- meanFilter pimg 3
  --vimg <- varianceFilter pimg 2
  --rimg <- meanRegions pimg 5
  --aimg <- adaptiveThreshold pimg 3
  --simg <- sauvolaThreshold pimg 4
  --fimg <- fengThreshold pimg 3
  --withPixelImage pimg $ \i -> do
  --eimg <- createEdgeImage 16 16 16 16 16 8 pimg
  --saveImage targetFile $ drawEdges eimg 16 img
  --
  --ps <- CVSU.getAllPixels pimg
  --nimg <- createFromPixels (width pimg) (height pimg) ps
  --let
    --left (x1,_,_,_) = x1
    --top (_,y1,_,_) = y1
    --cs = columnwiseChanges forest
    --bs = equivalenceBoxes 5 $ stripeEquivalences cs
    --bs2 = joinBoxes 5 $ (sortBy (comparing left)) $ (sortBy (comparing top)) bs
  --saveImage "normal.png" nimg
  --saveImage "mean.png" mimg
  --saveImage "variance.png" vimg
  --saveImage "adaptive.png" aimg
  --saveImage targetFile simg -- "sauvola.png"
  --saveImage "feng.png" fimg
  --saveImage "regions.png" rimg
  --saveImage targetFile $ drawBoxes (0,1,0) bs2 $ drawBoxes (0,0,1) bs $
  --  drawChanges cs img -- drawChanges cs $ drawBlocks forest
-- drawBoxes 3 bs $
  --drawBlocks forest $ drawEdges eimg nimg
