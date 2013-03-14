module CVSU.QuadTree
( QuadTree(..)
, quadTreeHasEdge
, quadTreeHasVEdge
, quadTreeHasHEdge
, quadTreeChildStat
, quadTreeNeighborhoodStat
, quadTreeDivide
, quadTreeNeighbors
, quadTreeEdgeResponse
, quadTreeChildEdgeResponse
, quadTreeSegmentInit
, quadTreeSegmentUnion
, quadTreeSegmentFind
) where

import CVSU.Bindings.Types
import CVSU.Bindings.QuadTree
import CVSU.Types

import Foreign.Ptr

data QuadTree = EmptyQuadTree |
  QuadTree
  { quadTreePtr :: Ptr C'quad_tree
  , quadTreeX :: Int
  , quadTreeY :: Int
  , quadTreeSize :: Int
  , quadTreeLevel :: Int
  , quadTreeStat :: Statistics
  , quadTreeEdge :: ForestEdge
  , quadTreeSegment :: ForestSegment
  , quadTreeChildNW :: QuadTree
  , quadTreeChildNE :: QuadTree
  , quadTreeChildSW :: QuadTree
  , quadTreeChildSE :: QuadTree
  } deriving Eq

quadTreeHasEdge  = edgeFound . quadTreeEdge
quadTreeHasVEdge t =
  (quadTreeHasEdge t) && ((==DirV) $ edgeDir $ quadTreeEdge t)
quadTreeHasHEdge t =
  (quadTreeHasEdge t) && ((==DirH) $ edgeDir $ quadTreeEdge t)

quadTreeFromPtr :: Ptr C'quad_tree -> IO QuadTree
quadTreeFromPtr ptree
  | ptree == nullPtr = return EmptyQuadTree
  | otherwise        = do
    (C'quad_tree x y s l stat seg edge _ _ _ nw ne sw se _ _ _ _ _ _ _ _ _ _) <- peek ptree
    tnw <- quadTreeFromPtr nw
    tne <- quadTreeFromPtr ne
    tsw <- quadTreeFromPtr sw
    tse <- quadTreeFromPtr se
    return $ QuadTree ptree
      (fromIntegral x)
      (fromIntegral y)
      (fromIntegral s)
      (fromIntegral l)
      (hStatistics stat)
      (hForestEdge edge)
      (hForestSegment seg)
      tnw tne tsw tse

-- | Creates an image tree from a list item by casting and converting
quadTreeFromListItem :: Ptr C'list_item -> IO QuadTree
quadTreeFromListItem pitem = do
  item <- peek pitem
  ptree <- peek $ castPtr $ c'list_item'data item
  quadTreeFromPtr $ castPtr ptree

-- | Divides the tree in four equal parts in quad-tree fashion. Causes side
--   effects as the original tree will have four child trees after this
--   operation.
quadTreeDivide :: QuadForest -> QuadTree -> IO [QuadTree]
quadTreeDivide forest tree =
  withForeignPtr (quadForestPtr forest) $ \pforest -> do
    r <- c'quad_tree_divide pforest (quadTreePtr tree)
    if r /= c'SUCCESS
      then error $ "Divide tree failed with " ++ (show r)
      else do
        ctree <- peek (quadTreePtr tree)
        liftM (filter (/=EmptyQuadTree)) $ mapM quadTreeFromPtr
          [ c'quad_tree'nw ctree
          , c'quad_tree'ne ctree
          , c'quad_tree'sw ctree
          , c'quad_tree'se ctree ]

quadTreeChildStat :: QuadForest -> QuadTree -> IO [Statistics]
quadTreeChildStat f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      allocChildArray :: IO (Ptr C'quad_tree)
      allocChildArray = mallocArray 4
    pchildren <- allocChildArray
    r <- c'quad_tree_get_child_statistics pforest (quadTreePtr t) pchildren
    if r /= c'SUCCESS
      then error $ "Get child statistics failed with " ++ (show r)
      else do
        children <- (peekArray 4 pchildren)
        return $ map (hStatistics.c'quad_tree'stat) children

quadTreeNeighborhoodStat :: QuadForest -> Double -> QuadTree -> IO Statistics
quadTreeNeighborhoodStat f m t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      stat = C'statistics 0 0 0 0 0 0
    with stat $ \pstat -> do
      r <- c'quad_tree_get_neighborhood_statistics pforest (quadTreePtr t) pstat (realToFrac m)
      if r /= c'SUCCESS
         then error $ "Get neighborhood statistics failed with " ++ (show r)
         else do
           nstat <- peek pstat
           return $ hStatistics nstat

-- | Finds the current neighbors of the given tree.
quadTreeNeighbors :: QuadTree -> IO [QuadTree]
quadTreeNeighbors tree = do
  flist <- allocList
  withForeignPtr flist $ \plist -> do
    r <- c'quad_tree_get_neighbors plist (quadTreePtr tree)
    if r /= c'SUCCESS
      then error $ "Getting tree neighbors failed with " ++ (show r)
      else createList plist quadTreeFromListItem

quadTreeEdgeResponse :: QuadForest -> QuadTree -> IO (Double,Double)
quadTreeEdgeResponse f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
        dx :: CDouble
        dx = 0
        dy :: CDouble
        dy = 0
    with dx $ \pdx ->
      with dy $ \pdy -> do
        r <- c'quad_tree_get_edge_response pforest (quadTreePtr t) pdx pdy
        if r /= c'SUCCESS
          then error $ "Get tree edge response failed with " ++ (show r)
          else do
            rdx <- peek pdx
            rdy <- peek pdy
            return (realToFrac rdx, realToFrac rdy)
            --return $ sqrt $ (realToFrac rdx)**2 + (realToFrac rdy)**2

quadTreeChildEdgeResponse :: QuadForest -> QuadTree -> IO [(Double,Double)]
quadTreeChildEdgeResponse f t =
  withForeignPtr (quadForestPtr f) $ \pforest -> do
    let
      allocDArray :: IO (Ptr CDouble)
      allocDArray = mallocArray 4
    pdx <- allocDArray
    pdy <- allocDArray
    r <- c'quad_tree_get_child_edge_response pforest (quadTreePtr t) pdx pdy
    if r /= c'SUCCESS
      then error $ "Get child edge response failed with " ++ (show r)
      else do
        xs <- (peekArray 4 pdx)
        ys <- (peekArray 4 pdy)
        return $ zip (map realToFrac xs) (map realToFrac ys)

-- | Initializes a disjunctive set for the tree by setting the parent to
--   self and rank to zero.
quadTreeSegmentInit :: QuadTree -> IO QuadTree
quadTreeSegmentInit t = do
  c'quad_tree_segment_create (quadTreePtr t)
  quadTreeFromPtr $ quadTreePtr t

-- | Union part of the Union-Find disjunctive set algorithm. Creates a union
--   of two sets by assigning the parent of the higher ranking tree as the
--   parent of the union. As the result, the two trees in question will have
--   the same classId. Loses persistency, as the classId is changed in the
--   underlying c structure, so previous versions of the trees that still
--   point to the same c structure will have different classId.
quadTreeSegmentUnion :: QuadTree -> QuadTree -> IO ()
quadTreeSegmentUnion t1 t2 =
  c'quad_tree_segment_union (quadTreePtr t1) (quadTreePtr t2)

-- | Creates a union of two segments that are larger than trees.
forestSegmentUnion :: ForestSegment -> ForestSegment -> IO ()
forestSegmentUnion s1 s2 =
  c'quad_forest_segment_union (segmentPtr s1) (segmentPtr s2)

-- | Find part of the Union-Find disjunctive set algorithm. Finds the pointer
--   of the parent tree, caches it in the c structure, and creates an Integer
--   from the pointer. May cause side effects as the parent pointer is
--   cached to shorten the search paths for better efficiency.
quadTreeSegmentFind :: QuadTree -> IO (Integer)
quadTreeSegmentFind t = do
  tid <- c'quad_tree_segment_get (quadTreePtr t)
  return $ fromIntegral tid
