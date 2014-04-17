{-#LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts,
            FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module CVSU.Graph
( Node(..)
, nodeNeighbors
, Link(..)
, CGraph(..)
, newCGraph
, Graph(..)
, GraphNeighborhood(..)
, graphCreate
, graphCreateFromImage
, graphAddAttribute
, graphGetAttribute
) where

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import CVSU.Bindings.Attribute
import CVSU.Bindings.List
import CVSU.Bindings.Graph
import CVSU.Bindings.Set

import CVSU.Types
import CVSU.TypedPointer
import CVSU.Attribute
import CVSU.List
import CVSU.PixelImage
import CVSU.Set

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Concurrent
import System.IO.Unsafe

import Control.Monad (liftM,foldM,filterM,mapM)
import Data.IORef

data Node a =
  Node
  { nodePtr :: !(Ptr C'node)
  , nodePosition :: (Double, Double)
  , nodeOrientation :: Double
  , nodeScale :: Int
  , nodeAttribute :: Attribute a
  }

instance (AttribValue a, Eq a, Eq (Key a)) => Eq (Node a) where
  (==) a b =
      ((nodePosition a) == (nodePosition b) &&
       (nodeOrientation a) == (nodeOrientation b) &&
       (nodeScale a) == (nodeScale b) &&
       (nodeAttribute a) == (nodeAttribute b))

instance (Show a, AttribValue a) => Show (Node a) where
  show (Node _ (x,y) _ _ a) = show (x, y, attribValue a)

nullAttribute :: Attribute ()
nullAttribute = unsafePerformIO $ attribCreateNull

nodeNull :: Node ()
nodeNull = Node nullPtr (0,0) 0 0 AttribUnit

nodeEmpty :: Ptr C'node -> IO (Node ())
nodeEmpty pnode
  | pnode == nullPtr = do
    return nodeNull
  | otherwise        = do
    (C'node x y o s _ _) <- peek pnode
    return $ Node pnode
        (realToFrac x, realToFrac y)
        (realToFrac o)
        (fromIntegral s)
        AttribUnit

nodeFromListItem :: AttribValue a => Attribute a -> Ptr C'list_item
    -> IO (Node a)
nodeFromListItem attrib pitem = do
  item <- peek pitem
  createAttributed attrib $ castPtr $ c'list_item'data item

--nodeLinks :: Node -> [Link]

nodeNeighbors :: AttribValue a => Node a -> IO [Node a]
nodeNeighbors node = do
  (C'link_list pheads _ n) <- peek $ p'node'links $ nodePtr node
  pheads::[Ptr C'link_head] <- peekArray (fromIntegral n) pheads
  pheads' <- filterM checkNull pheads
  heads::[C'link_head] <- mapM peek pheads'
  others::[C'link_head] <- mapM peek $ map c'link_head'other heads
  mapM (createAttributed (nodeAttribute node)) $ map c'link_head'origin others
  where
    checkNull :: Ptr C'link_head -> IO (Bool)
    checkNull p = return (p /= nullPtr)
    peekHead :: [C'link_head] -> Ptr C'link_head -> IO [C'link_head]
    peekHead hs ph
      | ph == nullPtr = return hs
      | otherwise = do
        h <- peek ph
        return (h:hs)

instance (AttribValue a) => Attributable Node a where
  type PAttributable Node a = Ptr C'node
  {-
  createEmpty pnode
    | pnode == nullPtr = do
      return $ Node nullPtr (0,0) 0 0 AttribUnit
    | otherwise        = do
      (C'node x y o s _ _) <- peek pnode
      return $ Node pnode
          (realToFrac x, realToFrac y)
          (realToFrac o)
          (fromIntegral s)
          AttribUnit
  -}
  createAttributed attrib pnode
    | pnode == nullPtr = do
      a <- attribCreateNull
      return $ Node nullPtr (0,0) 0 0 a
    | otherwise = do
      (C'node x y o s attriblist _) <- peek pnode
      with attriblist $ \pattriblist -> do
        nattrib <- attribGet attrib pattriblist
        return $ Node pnode
            (realToFrac x, realToFrac y)
            (realToFrac o)
            (fromIntegral s)
            nattrib

  addAttribute attrib node = attribAdd attrib (p'node'attributes $ nodePtr node)

  getAttribute attrib node =
    liftM attribValue $ attribGet attrib (p'node'attributes $ nodePtr node)

  setAttribute attrib value (Node ptr p o s _) = do
    nattrib <- attribSet attrib value (p'node'attributes ptr)
    return $ Node ptr p o s nattrib

instance (AttribValue a, AttribValue b) => Extendable Node a b where
  type Target Node a b = Node (a,b)
  extendWithAttrib attrib2 (node@(Node pnode (x,y) o s attrib1)) = do
    attrib2' <- addAttribute attrib2 node
    return (Node pnode (x,y) o s
        (PAttribPair((attribPtr    attrib1, attribPtr    attrib2'),
                     (attribKey    attrib1, attribKey    attrib2'),
                     (attribValue  attrib1, attribValue  attrib2'))))

data Link =
  Link
  { linkPtr :: !(Ptr C'link)
  , linkHead :: !(Ptr C'link_head)
  , linkWeight :: Double
  , linkFrom :: Node ()
  , linkTo :: Node ()
  -- , linkAttribute :: a
  } deriving Eq

--emptyNode :: (AttribValue ()) => Ptr C'node -> IO (Node ())
--emptyNode = createEmpty

linkFromPtr :: Ptr C'link -> IO (Link)
linkFromPtr plink
  | plink == nullPtr = return $
      Link nullPtr nullPtr 0 nodeNull nodeNull
  | otherwise        = do
    (C'link a b w attrlist) <- peek plink
    nodea <- nodeEmpty $ c'link_head'origin a
    nodeb <- nodeEmpty $ c'link_head'origin b
    return $ Link plink nullPtr (realToFrac w) nodea nodeb

-- add key and attributable
linkFromListItem :: Ptr C'list_item -> IO (Link)
linkFromListItem pitem = do
  item <- peek pitem
  linkFromPtr $ castPtr $ c'list_item'data item

--linkGetOrigin :: Link -> Node

--linkGetOther :: Link -> Node

data CGraph = CGraph{ graphPtr :: IORef(ForeignPtr C'graph) }

newCGraph :: IO CGraph
newCGraph = do
  pgraph <- c'graph_alloc
  if pgraph /= nullPtr
    then do
      fgraph <- newForeignPtr pgraph (c'graph_free pgraph)
      rgraph <- newIORef fgraph
      return $ CGraph rgraph
    else error "Memory allocation failed in newCGraph"

data Graph n =
  Graph
  { cgraph :: !CGraph
  , nodes :: [Node n]
  , links :: [Link]
  }

data GraphNeighborhood =
  Neighborhood4 |
  Neighborhood8 deriving (Eq,Show)

cNeighborhood :: GraphNeighborhood -> C'graph_neighborhood
cNeighborhood n
  | n == Neighborhood4 = c'NEIGHBORHOOD_4
  | n == Neighborhood8 = c'NEIGHBORHOOD_8
  | otherwise          = c'NEIGHBORHOOD_0

-- | Allocates memory for a graph structure and creates a foreign pointer
graphAlloc :: IO (ForeignPtr C'graph)
graphAlloc = do
  ptr <- c'graph_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'graph_free ptr)
    else error "Memory allocation failed in graphAlloc"

doGraphCreate :: (AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) =>
    Int -> Int -> Attribute a -> CGraph -> IO ()
doGraphCreate snode slink attrib cg =
  modifyIORef (graphPtr cg) $ \fgraph -> unsafePerformIO $ do
    withForeignPtr fgraph $ \pgraph ->
      withForeignPtr (attribPtr attrib) $ \pattr -> do
        r <- c'graph_create
                 pgraph
                 (fromIntegral snode)
                 (fromIntegral slink)
                 pattr
        if r /= c'SUCCESS
          then error $ "Failed to create graph with " ++ (show r)
          else return fgraph

-- | Creates an empty graph. In the underlying structure, memory will be
--   allocated for the given amount of nodes and links.
graphCreate :: (AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) =>
    Int -> Int -> Attribute a -> CGraph -> IO (Graph a)
graphCreate snode slink attrib cg = do
  doGraphCreate snode slink attrib cg
  graphFromPtr cg attrib

graphFromPtr :: AttribValue a => CGraph -> Attribute a -> IO (Graph a)
graphFromPtr cg attrib = do
  fgraph <- readIORef $ graphPtr cg
  withForeignPtr fgraph $ \pgraph -> do
    nodes <- createList (p'graph'nodes pgraph) (nodeFromListItem attrib)
    links <- createList (p'graph'links pgraph) (linkFromListItem) -- key
    return $ Graph cg nodes links

doGraphCreateFromImage ::
    (Num a, AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) =>
    PixelImage -> Int -> Int -> Int -> Int -> GraphNeighborhood -> Attribute a
    -> CGraph -> IO ()
doGraphCreateFromImage image ox oy sx sy n attrib cg = do
  modifyIORef (graphPtr cg) $ \fgraph -> unsafePerformIO $ do
    withForeignPtr fgraph $ \pgraph ->
      withForeignPtr (imagePtr image) $ \pimage ->
        withForeignPtr (attribPtr attrib) $ \pattrib -> do
          r <- c'graph_create_from_image
              pgraph
              pimage
              (fromIntegral ox)
              (fromIntegral oy)
              (fromIntegral sx)
              (fromIntegral sy)
              (cNeighborhood n)
              pattrib
          if r /= c'SUCCESS
            then error $ "Failed to create graph from image with " ++ (show r)
            else return fgraph

-- | Creates a regular grid graph from an image. The step in pixels between
--   grid rows and cols can be given, like also the neighborhood type and the
--   label for the attribute that will be used for storing the pixel values.
graphCreateFromImage ::
    (Num a, AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) =>
    PixelImage -> Int -> Int -> Int -> Int -> GraphNeighborhood -> Attribute a
    -> CGraph -> IO (Graph a)
graphCreateFromImage image ox oy sx sy n attrib cg = do
  doGraphCreateFromImage image ox oy sx sy n attrib cg
  graphFromPtr cg attrib
  --fgraph <- graphAlloc
  --modifyIORef cgraph $ fgraph ->
  --withForeignPtr fgraph $ \pgraph ->
  --  withForeignPtr (imagePtr image) $ \pimage ->
  --    withForeignPtr (attribPtr attrib) $ \pattrib -> do
  --      r <- c'graph_create_from_image
  --          pgraph
  --          pimage
  --          (fromIntegral offsetx)
  --          (fromIntegral offsety)
  --          (fromIntegral stepx)
  --          (fromIntegral stepy)
  --          (cNeighborhood neighborhood)
  --          pattrib
  --      if r /= c'SUCCESS
  --        then error $ "Failed to create graph from image with " ++ (show r)
  --        else graphFromPtr fgraph attrib

doGraphAddAttribute ::
    (AttribValue a, PAttribValue a ~ ForeignPtr f, AttribValue b) =>
    Attribute a -> Graph b -> IO ()
doGraphAddAttribute attrib (Graph cg ns _) =
  modifyIORef (graphPtr cg) $ \fgraph -> unsafePerformIO $ do
    withForeignPtr (attribPtr attrib) $ \pattrib -> do
      mapM_ (extendWithAttrib attrib) $ ns
      return fgraph

graphAddAttribute ::
    (AttribValue a, PAttribValue a ~ ForeignPtr f, AttribValue b) =>
    Attribute a -> Graph b -> IO (Graph (b,a))
graphAddAttribute attrib graph = do
  doGraphAddAttribute attrib graph
  pairAttrib <- attributePair (nodeAttribute $ head $ nodes graph) attrib
  graphFromPtr (cgraph graph) pairAttrib

graphGetAttribute ::
    (AttribValue a, AttribValue b) =>
    Attribute a -> Graph b -> IO (Graph a)
graphGetAttribute attrib graph = do
  graph' <- graphFromPtr (cgraph graph) attrib
  modifyIORef (graphPtr $ cgraph graph) $ \fgraph -> fgraph
  return graph'
