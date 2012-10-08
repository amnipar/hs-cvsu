{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import CVSU.Types
import CVSU.PixelImage as CVSU
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

import System.IO.Unsafe

import Debug.Trace

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


-- read cols one by one
-- assign id (carry previous id in a parameter)
-- compare with all stripes of previous col
-- if equal, add equivalence to relation list
-- need to know:
--  *last id
--  *stripes in previous col
--  *next stripe
stripeEquivalences :: [[(Int,Int,Int)]] -> ([(Int,Int)],[(Int,(Int,Int,Int))])
stripeEquivalences cs = (finalEqs,finalStripes)
  where
    (initId,initEq,initStripes) = foldl addStripe (0,[],[]) (head cs)
    (_,finalEqs,finalStripes,_) = foldl handleCol (initId,initEq,[],initStripes) (tail cs)
    addStripe (lastId,eqs,stripes) stripe = (lastId+1,eqs,((lastId+1,stripe):stripes))
    addEquivalence eqs ((aid,a),(bid,b))
      | isEquivalent a b = ((aid,bid):eqs)
      | otherwise = eqs
    isEquivalent (_,ay1,ay2) (_,by1,by2) = (by1 <= ay2) && (by2 >= ay1)
    handleCol (lastId,eqs,stripes,lastStripes) col = (newLastId,newEqs,stripes++lastStripes,newStripes)
      where
        (newLastId,_,newStripes) = foldl addStripe (lastId,eqs,[]) col
        newEqs = foldl addEquivalence eqs pairs
        pairs = [(a,b) | a <- lastStripes, b <- newStripes]

-- check stripes one by one
-- compare id to list of id's in each box
-- if id matches, add to list of id's, add to list of stripes, increase bounding box
-- when first match is found, search should be terminated -> make own recursion
-- add new box always to beginning of list, so search terminates sooner
-- need to carry around the previous list and remaining list and then concatenate?
equivalenceComponents :: ([(Int,Int)],[(Int,(Int,Int,Int))]) -> [((Int,Int,Int,Int),([Int],[(Int,Int,Int)]))]
equivalenceComponents (eqs,stripes) = foldl (addComponents eqs) [] stripes
  where
    addComponents :: [(Int,Int)] -> [((Int,Int,Int,Int),([Int],[(Int,Int,Int)]))] -> (Int,(Int,Int,Int)) -> [((Int,Int,Int,Int),([Int],[(Int,Int,Int)]))]
    addComponents eqs cs s = findComponent eqs s [] cs
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
    findComponent _ s _ [] = [initComponent s]
    findComponent eqs s@(sid,_) cas (c@(_,(ids,_)):cbs)
      | not $ null $ filter (findEq eqs sid) ids = (expandComponent c s):(cas ++ cbs)
      | otherwise = findComponent eqs s (cas ++ [c]) cbs
    initComponent (sid,s@(sx,sy1,sy2)) = ((sx,sy1,sx,sy2),([sid],[s]))
    expandComponent ((x1,y1,x2,y2),(ids,stripes)) (sid,s@(sx,sy1,sy2)) =
      (((min x1 sx),(min y1 sy1),(max x2 sx),(max y2 sy2)),(sid:ids,s:stripes))

equivalenceBoxes :: ([(Int,Int)],[(Int,(Int,Int,Int))]) -> [(Int,Int,Int,Int)]
equivalenceBoxes (eqs,stripes) = map fst $ equivalenceComponents (eqs,stripes)

-- for each stripe in previous col
-- check every stripe in current col
-- if they are equivalent, add equivalence relation

columnwiseChanges :: ImageForest Stat -> [[(Int,Int,Int)]]
columnwiseChanges f@(ImageForest ptr _ r c _ ts) = colLines
  where
    ds = map (statDev . T.value . block) ts
    avgDev = floor $ (fromIntegral $ sum ds) / (fromIntegral $ length ds)
    toLine f (x,y) = liftM toL $ getTree f (x,y)
      where
        toL (ImageTree _ (ImageBlock n e s w (Stat(_,d))) _ _ _ _) =
          ((round $ (e+w)/2, round n, round s),d)
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

joinBoxes :: Int -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
joinBoxes s bs = combine bs
  where
    combine [] = []
    combine (b:bs)
      | isSmall b = (combine bs) ++ (newBox (b:(pick b bs)))
      | isLong b = combine bs
      | otherwise = (b:(combine bs))
    dx (x1,_,x2,_) = x2-x1
    dy (_,y1,_,y2) = y2-y1
    isTooSmall b = dx b < 5*s || dy b < 5*s
    isSmall b = dx b < 12*s || dy b < 6*s
    isLong b = dy b < 8*s && dx b > 24*s
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
      | w >= 12*s && w <= 36*s && h >= 6*s && h <= 12*s = [b]
      | w > 36*s = []
      | h > 12*s = pick a bs
      | otherwise = b:(pick a bs)
      where
        w = bx2-ax1
        h = (max by2 ay2)-(min by1 ay1)

createFromPixels :: Int -> Int -> [((Int,Int),Float)] -> IO (Image GrayScale D32)
createFromPixels w h ps = do
  i <- create (w,h)
  mapM_ (sp i) ps
  return $ stretchHistogram i
  where
        minV = minimum $ map snd ps
        maxV = maximum $ map snd ps
        sp i ((x,y),v) = setPixel (x,y) v i
        --i = imageFromFunction (w,h) (const 0)

drawChanges :: [[(Int,Int,Int)]] -> Image GrayScale D32 -> Image GrayScale D32
drawChanges ls i =
  i
  <## concat [[lineOp 1 1 (x,y1) (x,y2) | (x,y1,y2) <- cs] | cs <- ls]

drawBoxes :: Int -> [(Int,Int,Int,Int)] -> Image GrayScale D32 -> Image GrayScale D32
drawBoxes dx bs i =
  i
  <## [rectOp 1 1 r | r <- map (toRect dx) bs]
  where
    toRect dx (x1,y1,x2,y2) = mkRectangle (x1-dx,y1) (x2-x1+2*dx,y2-y1)

drawEdges :: EdgeImage -> Image GrayScale D32 -> Image GrayScale D32
drawEdges eimg i =
  i
  <## [lineOp 1 1 (x,y) (x+8,y) | (Edge x y _ v) <- he] -- ((abs v) / maxV)
  <## [lineOp 1 1 (x,y) (x,y+8) | (Edge x y _ v) <- ve]
  where
        maxV = maximum $ map (abs . E.value) (he ++ ve)
        he = filter ((>1) . abs . E.value) $ concat $ hedges eimg
        ve = filter ((>1) . abs . E.value) $ concat $ vedges eimg

drawBlocks :: ImageForest Stat -> Image GrayScale D32 -> Image GrayScale D32
drawBlocks (ImageForest ptr _ _ _ _ ts) i =
  i
  <## [rectOp 1 1 r | r <- map toRect $ filter ((>avgDev) . statDev . T.value) $ map block ts]
  where
    ds = map (statDev . T.value . block) ts
    avgDev = floor $ (fromIntegral $ sum ds) / (fromIntegral $ length ds)
    toRect (ImageBlock n e s w _) = mkRectangle (round w, round n) (round (s-n), round (e-w))

main = do
  img <- readFromFile "rengas.jpg"
  pimg <- readPixelImage "rengas.jpg"
  --withPixelImage pimg $ \i -> do
  --eimg <- createEdgeImage 8 8 8 8 8 4 pimg
  forest <- createForest pimg (6,6)
  --ps <- CVSU.getAllPixels pimg
  --nimg <- createFromPixels (width pimg) (height pimg) ps
  let
    cs = columnwiseChanges forest
    bs = equivalenceBoxes $ stripeEquivalences cs
    --bs = joinBoxes 6 $ columnwiseBoxing 6 cs
  saveImage "result.png" $ drawBoxes 3 bs $ drawChanges cs img -- drawChanges cs $ drawBlocks forest

  --drawBlocks forest $ drawEdges eimg nimg
