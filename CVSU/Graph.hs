module CVSU.Graph
( Attribute(..)
, attributeCreate
-- , attributeLabel
, Node(..)
, Link(..)
, Graph(..)
, GraphNeighborhood(..)
, graphCreate
, graphFromImage
) where

import CVSU.Bindings.Types
import CVSU.Bindings.List
import CVSU.Bindings.Graph

import CVSU.Types
import CVSU.TypedPointer
import CVSU.List
import CVSU.PixelImage

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Concurrent

data Attribute a =
  NoSuchAttribute |
  Attribute
  { attributePtr :: !(ForeignPtr C'attribute)
  , attributeKey :: Int
  , attributeValue :: a
  }

attributeAlloc :: IO (ForeignPtr C'attribute)
attributeAlloc = do
  ptr <- c'attribute_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'attribute_free ptr)
    else error "Memory allocation failed in attributeAlloc"

attributeCreate :: Pointable a => Int -> a -> IO (Attribute a)
attributeCreate attrKey attrValue = do
  ftptr <- intoTypedPointer attrValue
  fattr <- attributeAlloc
  withForeignPtr fattr $ \pattr ->
    withForeignPtr ftptr $ \ptptr -> do
      r <- c'attribute_create pattr (fromIntegral attrKey) ptptr
      if r /= c'SUCCESS
        then error $ "Failed to create attribute with " ++ (show r)
        else attributeFromFPtr fattr

attributeFromFPtr :: Pointable a => ForeignPtr C'attribute -> IO (Attribute a)
attributeFromFPtr fattr =
  withForeignPtr fattr $ \pattr -> do
    C'attribute{
      c'attribute'key = k,
      c'attribute'value = tptr
    } <- peek pattr
    v <- convertFrom tptr
    return $ Attribute fattr (fromIntegral k) v

-- | Creates an attribute from a ptr; should be used only for attributes from
--   nodes and other elements, that don't need to be freed.
attributeFromPtr :: Pointable a => Ptr C'attribute -> IO (Attribute a)
attributeFromPtr pattr = do
  C'attribute{
    c'attribute'key = k,
    c'attribute'value = tptr
  } <- peek pattr
  v <- convertFrom tptr
  -- no need to free attributes from elements
  fattr <- newForeignPtr pattr (c'attribute_free nullPtr)
  return $ Attribute fattr (fromIntegral k) v

attributeFromList :: Pointable a => Int -> Ptr C'attribute_list
    -> IO (Attribute a)
attributeFromList key pattrlist = do
  pattr <- c'attribute_find pattrlist (fromIntegral key)
  if pattr /= nullPtr
     then attributeFromPtr pattr
     else return NoSuchAttribute

instance (Eq a) => Eq (Attribute a) where
  (==) a b
    | a == NoSuchAttribute || b == NoSuchAttribute = False
    | otherwise = (attributeKey a == attributeKey b) && 
                  (attributeValue a == attributeValue b)

-- | In order to be usable as attributes, a type must provide conversions from
--   and to typed pointers.
{-
class Attributable a where
  convertTo :: C'typed_pointer -> IO a
  convertFrom :: a -> IO C'typed_pointer
  -}
-- | Creates an attribute label. This is used for declaring the typeclass of
--   the attributes for use in functions.
--attributeLabel :: Int -> a -> IO (Attribute a)
--attributeLabel attrId attrVal = Attribute nullPtr attrId attrVal
{-
attributeGet :: Attribute a -> Node -> Attribute a
attributeGet attrLabel node =

attributeSet :: Attribute a -> Node -> a -> Node
attributeSet attrLabel node attrValue =

attributeSetCreate :: Attribute Set -> Int -> Node -> Node
attributeSetCreate attrLabel setLabel node =

-- | Creates a union of sets with the given label in a list of nodes.
attributeSetUnion :: Attribute Set -> Node -> [Node] -> Node
attributeSetUnion attrLabel node nodes =

-- | Compares an Attribute in two Nodes by using a comparison function
attributeCompare :: Eq Attribute a =>
  Attribute a -> (Attribute a -> Attribute a -> Bool) -> Node -> Node -> Bool
attributeCompare attrLabel attrEqual a b =
  attrEqual (attributeGet attrLabel a) (attributeGet attrLabel b)
-}
data Node a =
  Node
  { nodePtr :: !(Ptr C'node)
  , nodePosition :: (Double, Double)
  , nodeOrientation :: Double
  , nodeScale :: Int
  , nodeAttribute :: Attribute a
  } deriving Eq

instance (Show a) => Show (Node a) where
  show (Node _ (x,y) _ _ (Attribute _ key val)) = show (x,y,val)

nodeFromPtr :: Pointable a => Int -> Ptr C'node -> IO (Node a)
nodeFromPtr key pnode
  | pnode == nullPtr = return $ Node nullPtr (0,0) 0 0 NoSuchAttribute
  | otherwise        = do
    (C'node x y o s attrs _) <- peek pnode
    with attrs $ \pattrs -> do
      attr <- attributeFromList key pattrs
      return $ Node pnode 
          (realToFrac x, realToFrac y)
          (realToFrac o)
          (fromIntegral s)
          attr

nodeFromListItem :: Pointable a => Int -> Ptr C'list_item -> IO (Node a)
nodeFromListItem key pitem = do
  item <- peek pitem
  --pnode <- peek $ 
  nodeFromPtr key $ castPtr $ c'list_item'data item

--nodeLinks :: Node -> [Link]
--nodeNeighbors :: Node -> [Node]

data Link =
  Link
  { linkPtr :: !(ForeignPtr C'link)
  , linkHead :: !(ForeignPtr C'link_head)
  , linkWeight :: Double
  -- , linkAttribute :: a
  } deriving Eq

--linkGetOrigin :: Link -> Node

--linkGetOther :: Link -> Node

data Graph n =
  Graph
  { graphPtr :: !(ForeignPtr C'graph)
  , nodes :: [Node n]
  , links :: [Link]
  } deriving Eq

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

-- | Creates an empty graph. In the underlying structure, memory will be
--   allocated for the given amount of nodes and links.
graphCreate :: Pointable a => Int -> Int -> Attribute a -> IO (Graph a)
graphCreate nodeSize linkSize attrLabel = do
  fgraph <- graphAlloc
  withForeignPtr fgraph $ \pgraph ->
    withForeignPtr (attributePtr attrLabel) $ \pattr -> do
      r <- c'graph_create pgraph
               (fromIntegral nodeSize)
               (fromIntegral linkSize)
               pattr
      if r /= c'SUCCESS
        then error $ "Failed to create graph with " ++ (show r)
        else graphFromPtr fgraph (attributeKey attrLabel)

graphFromPtr :: Pointable a => ForeignPtr C'graph -> Int -> IO (Graph a)
graphFromPtr fgraph key =
  withForeignPtr fgraph $ \pgraph -> do
    nodes <- createList (p'graph'nodes pgraph) (nodeFromListItem key)
    return $ Graph fgraph nodes []

-- | Creates a regular grid graph from an image. The step in pixels between 
--   grid rows and cols can be given, like also the neighborhood type and the
--   label for the attribute that will be used for storing the pixel values.
graphFromImage :: (Num a, Pointable a) => PixelImage -> Int -> Int -> Int
    -> Int -> GraphNeighborhood -> Attribute a -> IO (Graph a)
graphFromImage image offsetx offsety stepx stepy neighborhood attrLabel = do
  fgraph <- graphAlloc
  withForeignPtr fgraph $ \pgraph ->
    withForeignPtr (imagePtr image) $ \pimage ->
      withForeignPtr (attributePtr attrLabel) $ \pattr -> do
        r <- c'graph_create_from_image
                 pgraph
                 pimage
                 (fromIntegral offsetx)
                 (fromIntegral offsety)
                 (fromIntegral stepx)
                 (fromIntegral stepy)
                 (cNeighborhood neighborhood)
                 pattr
        if r /= c'SUCCESS
          then error $ "Failed to create graph from image with " ++ (show r)
          else graphFromPtr fgraph (attributeKey attrLabel)
