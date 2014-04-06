{-#LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, 
            UndecidableInstances #-}
module CVSU.Graph
( Attributable(..)
, Attribute(..)
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
import System.IO.Unsafe

class (Pointable a) => AttribValue a where
  type Key a :: *
  data Attribute a

-- | A class for types that can be equipped with attributes
class Attributable a where
  type Origin a :: *
  createEmpty :: Origin a -> IO (a ())
  createWithValue :: (AttribValue b) => Key b -> b -> Origin a -> IO (a b)
  createWithAttrib :: (AttribValue b) => Attribute b -> Origin a -> IO (a b)

class (AttribValue e) => Expandable e where
  type Source e :: *
  type Target e :: a -> *
  extendWithAttrib :: (AttribValue a) => (e a) -> Source e -> Target e a

{-
how should this work?

-there are many entities that can have attribs: graphs, nodes, links, heads.
-some of the attribs may be 'hidden'.
-we may 'declare' attribs for elems and they are retrieved if present.
-we may add new attribs and extend elems with attribs with new attribs.
-certain operations 'expect' certain attribs and these are declared in type
 signatures.
(Expandable a) => Graph a -> Attribute Set -> Graph (Target e Set)
-}

class Attributable a where
  type Key a :: *
  data Attribute a
  --attributePtr :: Attribute a -> (ForeignPtr C'attribute)
  attributeKey :: Attribute a -> Key a
  attributeValue :: Attribute a -> a
  nullAttribute :: Key a -> Attribute a
  nullKey :: Attribute a -> Bool
  createFrom :: (Key a) -> a -> IO (Attribute a)
  convertFrom :: (Key a) -> Ptr C'attribute_list -> IO (Attribute a)

nullAttrPtr = unsafePerformIO $ newForeignPtr nullPtr (c'attribute_free nullPtr)
{-
instance Attributable () where
  type Key () = ()
  data Attribute () = NoSuchAttribute
  attributePtr _ = nullAttrPtr
  attributeKey _ = ()
  attributeValue _ = ()
  createFrom _ _ = return NoSuchAttribute
  convertFrom _ _ = return NoSuchAttribute
-}

instance (Pointable a) => Attributable a where
  type Key a = Int
  data Attribute a = 
    NoSuchAttribute |
    PAttribute
    { pointablePtr :: !(ForeignPtr C'attribute)
    , pointableKey :: Key a
    , pointableValue :: a
    }
  --attributePtr a = pptr a
  attributeKey a = pointableKey a
  attributeValue a = pointableValue a
  nullAttribute _ = NoSuchAttribute
  nullKey a = (pointableKey a) == 0
  createFrom = attributeCreate
  convertFrom = attributeFromList

instance (Attributable a, b) => Attributable (a,b) where
  type Key (a,b) = (Int,Int)
  newtype Attribute (a,b) = (Attribute a, Attribute b)
  attributeKey (a,b) = (attributeKey a, attributeKey b)
  attributeValue (a,b) = (attributeValue a, attributeValue b)
  nullAttribute (k1,k2) = (nullAttribute k1, nullAttribute k2)
  nullKey (a,b) = k1 == 0 || k2 == 0
    where (k1,k2) = attributeKey (a,b)
  createFrom (k1,k2) (v1,v2) = do
    a1 <- createFrom k1 v1
    a2 <- createFrom k2 v2
    return $ (a1,a2)
  convertFrom (k1,k2) p = do
    a1 <- convertFrom k1 p
    a2 <- convertFrom k2 p
    return (a1,a2)

class Extendable e where
  type Source e :: *
  type Target e :: *
  extend :: e -> Source e -> Target e

instance (Pointable e) => Extendable e where
  type Source e = PAttribute
  type Target e = (PAttribute,PAttribute)
  extend :: PAttribute -> PAttribute -> (PAttribute,PAttribute)
  

attributeAlloc :: IO (ForeignPtr C'attribute)
attributeAlloc = do
  ptr <- c'attribute_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'attribute_free ptr)
    else error "Memory allocation failed in attributeAlloc"

attributeCreate :: Pointable a => Int -> a -> IO (Attribute a)
attributeCreate attrKey attrValue = do
  ftptr <- intoFTypedPointer attrValue
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
    v <- fromTypedPointer tptr
    return $ PointableAttribute fattr (fromIntegral k) v

-- | Creates an attribute from a ptr; should be used only for attributes from
--   nodes and other elements, that don't need to be freed.
attributeFromPtr :: Pointable a => Ptr C'attribute -> IO (Attribute a)
attributeFromPtr pattr = do
  C'attribute{
    c'attribute'key = k,
    c'attribute'value = tptr
  } <- peek pattr
  v <- fromTypedPointer tptr
  -- no need to free attributes from elements
  fattr <- newForeignPtr pattr (c'attribute_free nullPtr)
  return $ PointableAttribute fattr (fromIntegral k) v

attributeFromList :: Pointable a => Int -> Ptr C'attribute_list
    -> IO (Attribute a)
attributeFromList key pattrlist 
  | key == 0  = return $ nullAttribute key
  | otherwise = do
    pattr <- c'attribute_find pattrlist (fromIntegral key)
    if pattr /= nullPtr
      then attributeFromPtr pattr
      else return $ nullAttribute key

instance (Attributable a, Eq a, Eq (Key a)) => Eq (Attribute a) where
  (==) a b
    | nullKey a || nullKey b = False
    | otherwise = (attributeKey a == attributeKey b) && 
                  (attributeValue a == attributeValue b)

-- | In order to be usable as attributes, a type must provide conversions from
--   and to typed pointers.

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

instance (Show a, Attributable a) => Show (Node a) where
  show (Node _ (x,y) _ _ a) = show (x,y,attributeValue a)

nodeNull :: Attributable a => Key a -> Node a
nodeNull key = Node nullPtr (0,0) 0 0 $ nullAttribute key

nodeFromPtr :: Attributable a => Key a -> Ptr C'node -> IO (Node a)
nodeFromPtr key pnode
  | pnode == nullPtr = return $ nodeNull key
  | otherwise        = do
    (C'node x y o s attrs _) <- peek pnode
    with attrs $ \pattrs -> do
      attr <- convertFrom key pattrs
      return $ Node pnode 
          (realToFrac x, realToFrac y)
          (realToFrac o)
          (fromIntegral s)
          attr

nodeFromListItem :: Attributable a => Key a -> Ptr C'list_item -> IO (Node a)
nodeFromListItem key pitem = do
  item <- peek pitem
  --pnode <- peek $ 
  nodeFromPtr key $ castPtr $ c'list_item'data item

--nodeLinks :: Node -> [Link]
--nodeNeighbors :: Node -> [Node]

data Link =
  Link
  { linkPtr :: !(Ptr C'link)
  , linkHead :: !(Ptr C'link_head)
  , linkWeight :: Double
  , linkFrom :: Node ()
  , linkTo :: Node ()
  -- , linkAttribute :: a
  } deriving Eq

linkFromPtr :: Ptr C'link -> IO (Link)
linkFromPtr plink
  | plink == nullPtr = return $ 
      Link nullPtr nullPtr 0 (nodeNull 0) (nodeNull 0)
  | otherwise        = do
    (C'link a b w attrlist) <- peek plink
    nodea <- nodeFromPtr 0 $ c'link_head'origin a
    nodeb <- nodeFromPtr 0 $ c'link_head'origin b
    return $ Link plink nullPtr (realToFrac w) nodea nodeb

-- add key and attributable
linkFromListItem :: Ptr C'list_item -> IO (Link)
linkFromListItem pitem = do
  item <- peek pitem
  linkFromPtr $ castPtr $ c'list_item'data item

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
graphCreate :: Attributable a => Int -> Int -> Attribute a -> IO (Graph a)
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

graphFromPtr :: Attributable a => ForeignPtr C'graph -> Int -> IO (Graph a)
graphFromPtr fgraph key =
  withForeignPtr fgraph $ \pgraph -> do
    nodes <- createList (p'graph'nodes pgraph) (nodeFromListItem key)
    links <- createList (p'graph'links pgraph) (linkFromListItem) -- key
    return $ Graph fgraph nodes links

-- | Creates a regular grid graph from an image. The step in pixels between 
--   grid rows and cols can be given, like also the neighborhood type and the
--   label for the attribute that will be used for storing the pixel values.
graphFromImage :: (Num a, Attributable a) => PixelImage -> Int -> Int -> Int
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
