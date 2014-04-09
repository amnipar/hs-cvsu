{-#LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, 
UndecidableInstances, MultiParamTypeClasses, OverlappingInstances,
ScopedTypeVariables#-}
module CVSU.Graph
( AttribValue(..)
, Attributable(..)
, Extendable(..)
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
import CVSU.Set

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Concurrent
import System.IO.Unsafe

import Control.Monad (liftM)

-- | A class for types that can be used as attributes in attributable elements.
class AttribValue a where
  -- | The origin type on c side with which the attribute is linked with.
  --   Usually it is a tuple of ForeignPtr C'attribute.
  type PAttribValue a :: *
  -- | The type used as key in the attribute. Usually a tuple of Ints.
  type Key a :: *
  -- | The type storing the attribute fields. Usually a tuple containing
  --   Origin a, Key a, and a.
  data Attribute a
  -- | Returns the attribute origin (usually a foreign pointer).
  attribPtr :: Attribute a -> PAttribValue a
  -- | Returns the attribute key.
  attribKey :: Attribute a -> Key a
  -- | Returns the attribute value.
  attribValue :: Attribute a -> a
  -- | Checks whether the attribute is null/empty.
  attribIsNull :: Attribute a -> Bool
  -- | Creates a null/empty attribute.
  attribCreateNull :: IO (Attribute a)
  attribInit :: PAttribValue a -> Key a -> a -> Attribute a
  -- | Creates a standalone attribute with the given key and value. Intended
  --   mainly for label attributes used as parameter in other operations.
  attribCreate :: Key a -> a -> IO (Attribute a)
  -- | Adds an attribute to an attribute list. Intended for use via
  --   attributables, not directly.
  attribAdd :: Attribute a -> Ptr C'attribute_list -> IO (Attribute a)
  -- | Gets an attribute from an attribute list. Intended for use via
  --   attributables, not directly.
  attribGet :: Attribute a -> Ptr C'attribute_list -> IO (Attribute a)
  -- | Sets the value of an attribute in an attribute list. Intended for use via
  --   attributables, not directly.
  attribSet :: Attribute a -> a -> Ptr C'attribute_list -> IO (Attribute a)

data NoPAttribValue = NoPAttribValue
data NoKey = NoKey deriving Eq

-- | AttribValue instance for ()
instance AttribValue () where
  type PAttribValue () = NoPAttribValue
  type Key () = NoKey
  data Attribute () = AttribUnit
  attribPtr AttribUnit = NoPAttribValue
  attribKey AttribUnit = NoKey
  attribValue AttribUnit = ()
  attribIsNull AttribUnit = True
  attribCreateNull = return $ AttribUnit
  attribInit _ _ _ = AttribUnit
  attribCreate _ _ = return $ AttribUnit
  attribAdd _ _ = return $ AttribUnit
  attribGet _ _ = return $ AttribUnit
  attribSet _ _ _ = return $ AttribUnit

-- | AttribValue instance for Int
instance AttribValue Int where
  type PAttribValue Int = ForeignPtr C'attribute
  type Key Int = Int
  newtype (Attribute Int) = PAttribInt(PAttribValue Int, Key Int, Int)
  attribPtr (PAttribInt(p,_,_)) = p
  attribKey (PAttribInt(_,k,_)) = k
  attribValue (PAttribInt(_,_,v)) = v
  attribIsNull (PAttribInt(_,k,_)) = k == 0
  attribInit p k v = (PAttribInt(p,k,v))
  attribCreateNull = do
    p <- attributeNull
    return (PAttribInt(p, 0, pointableNull))
  attribCreate key value
    | key == 0  = attribCreateNull
    | otherwise = do
      ftptr <- pointableInto value
      -- this will be my responsibility to free
      fattr <- attributeAlloc
      withForeignPtr fattr $ \pattr ->
        withForeignPtr ftptr $ \ptptr -> do
          r <- c'attribute_create pattr (fromIntegral key) ptptr
          if r /= c'SUCCESS
            then error $ "Failed to create attribute with " ++ (show r)
            else do
              C'attribute{
                c'attribute'key = k,
                c'attribute'value = tptr
              } <- peek pattr
              v <- pointableFrom tptr
              return $ PAttribInt(fattr, (fromIntegral k), v)
  
  attribAdd attrib pattriblist = do
    withForeignPtr (attribPtr attrib) $ \pattrib -> do
      let
        pattrib2 :: Ptr C'attribute
        pattrib2 = nullPtr
      with pattrib2 $ \ppattrib2 -> do
        r <- c'attribute_add pattriblist pattrib ppattrib2
        if r == c'SUCCESS
          then do
            pattrib' <- peek ppattrib2
            (C'attribute k tptr) <- peek pattrib'
            v <- pointableFrom tptr
            -- not my responsibility to free, but the attriblist's owner's
            fattrib <- newForeignPtr pattrib' (c'attribute_free nullPtr)
            return $ PAttribInt(fattrib, (fromIntegral k), v)
          else attribCreateNull
  
  attribGet attrib pattriblist
    | attribKey attrib == 0  = attribCreateNull
    | otherwise = do
      pattrib <- c'attribute_find pattriblist (fromIntegral $ attribKey attrib)
      if pattrib /= nullPtr
         then do
           C'attribute{
             c'attribute'key = k,
             c'attribute'value = tptr
           } <- peek pattrib
           v <- pointableFrom tptr
           -- not my responsibility to free, but the attriblist's owner's
           fattrib <- newForeignPtr pattrib (c'attribute_free nullPtr)
           return $ PAttribInt(fattrib, (fromIntegral k), v)
         else attribCreateNull
  
  attribSet attrib value pattriblist = return attrib -- TODO: implement

-- | AttribValue instance for Set
instance AttribValue Set where
  type PAttribValue Set = ForeignPtr C'attribute
  type Key Set = Int
  newtype (Attribute Set) = PAttribSet(PAttribValue Set, Key Set, Set)
  attribPtr (PAttribSet(p,_,_)) = p
  attribKey (PAttribSet(_,k,_)) = k
  attribValue (PAttribSet(_,_,v)) = v
  attribIsNull (PAttribSet(_,k,_)) = k == 0
  attribInit p k v = (PAttribSet(p,k,v))
  attribCreateNull = do
    p <- attributeNull
    return (PAttribSet(p, 0, pointableNull))
  attribCreate key value
    | key == 0  = attribCreateNull
    | otherwise = do
      ftptr <- pointableInto value
      -- this will be my responsibility to free
      fattr <- attributeAlloc
      withForeignPtr fattr $ \pattr ->
        withForeignPtr ftptr $ \ptptr -> do
          r <- c'attribute_create pattr (fromIntegral key) ptptr
          if r /= c'SUCCESS
            then error $ "Failed to create attribute with " ++ (show r)
            else do
              C'attribute{
                c'attribute'key = k,
                c'attribute'value = tptr
              } <- peek pattr
              v <- pointableFrom tptr
              return $ PAttribSet(fattr, (fromIntegral k), v)
  
  attribAdd attrib pattriblist = do
    withForeignPtr (attribPtr attrib) $ \pattrib -> do
      let
        pattrib2 :: Ptr C'attribute
        pattrib2 = nullPtr
      with pattrib2 $ \ppattrib2 -> do
        r <- c'attribute_add pattriblist pattrib ppattrib2
        if r == c'SUCCESS
          then do
            pattrib' <- peek ppattrib2
            (C'attribute k tptr) <- peek pattrib'
            v <- pointableFrom tptr
            -- not my responsibility to free, but the attriblist's owner's
            fattrib <- newForeignPtr pattrib' (c'attribute_free nullPtr)
            return $ PAttribSet(fattrib, (fromIntegral k), v)
          else attribCreateNull
  
  attribGet attrib pattriblist
    | attribKey attrib == 0  = attribCreateNull
    | otherwise = do
      pattrib <- c'attribute_find pattriblist (fromIntegral $ attribKey attrib)
      if pattrib /= nullPtr
         then do
           C'attribute{
             c'attribute'key = k,
             c'attribute'value = tptr
           } <- peek pattrib
           v <- pointableFrom tptr
           -- not my responsibility to free, but the attriblist's owner's
           fattrib <- newForeignPtr pattrib (c'attribute_free nullPtr)
           return $ PAttribSet(fattrib, (fromIntegral k), v)
         else attribCreateNull
  
  attribSet attrib value pattriblist = return attrib -- TODO: implement

-- | AttribValue instance for a pair of Pointables
instance (AttribValue a, AttribValue b) => AttribValue (a,b) where
  type PAttribValue (a,b) = (PAttribValue a, PAttribValue b)
  type Key (a,b) = (Key a, Key b)
  newtype Attribute (a,b) = PAttribPair(PAttribValue (a,b), Key (a,b), (a,b))
  attribPtr (PAttribPair(p,_,_)) = p
  attribKey (PAttribPair(_,k,_)) = k
  attribValue (PAttribPair(_,_,v)) = v
  attribInit p k v = (PAttribPair(p,k,v))
  attribIsNull a = 
    (attribIsNull $ fstAttribute a) || (attribIsNull $ sndAttribute a)
  attribCreateNull = do
    attrib1 <- attribCreateNull
    attrib2 <- attribCreateNull
    return $ PAttribPair((attribPtr   attrib1, attribPtr   attrib2),
                         (attribKey   attrib1, attribKey   attrib2),
                         (attribValue attrib1, attribValue attrib2))
  
  attribCreate (key1,key2) (value1,value2) = do
    attrib1 <- attribCreate key1 value1
    attrib2 <- attribCreate key2 value2
    return $ PAttribPair((attribPtr    attrib1, attribPtr    attrib2),
                         (attribKey    attrib1, attribKey    attrib2),
                         (attribValue  attrib1, attribValue  attrib2))
  
  attribAdd attrib pattriblist = do
    attrib1 <- attribAdd (fstAttribute attrib) pattriblist
    attrib2 <- attribAdd (sndAttribute attrib) pattriblist
    return $ PAttribPair((attribPtr    attrib1, attribPtr    attrib2),
                         (attribKey    attrib1, attribKey    attrib2),
                         (attribValue  attrib1, attribValue  attrib2))
  
  attribGet attrib pattriblist = do
    attrib1 <- attribGet (fstAttribute attrib) pattriblist
    attrib2 <- attribGet (sndAttribute attrib) pattriblist
    return $ PAttribPair((attribPtr    attrib1, attribPtr    attrib2),
                         (attribKey    attrib1, attribKey    attrib2),
                         (attribValue  attrib1, attribValue  attrib2))
  
  attribSet attrib value pattriblist = return attrib -- TODO: implement

-- | Returns the first attribute of a pair
fstAttribute :: (AttribValue a, AttribValue b, AttribValue (a,b)) =>
    Attribute (a,b) -> Attribute a
fstAttribute (PAttribPair((p1,_),(k1,_),(v1,_))) = attribInit p1 k1 v1

-- | Returns the second attribute of a pair
sndAttribute :: (AttribValue a, AttribValue b, AttribValue (a,b)) =>
    Attribute (a,b) -> Attribute b
sndAttribute (PAttribPair((_,p2),(_,k2),(_,v2))) = attribInit p2 k2 v2

-- | A class for types that can be equipped with attributes.
class (AttribValue b) => Attributable a b where
  type PAttributable a b :: *
  -- | Creates an attributable element with no attributes
  createEmpty :: PAttributable a b -> IO (a ())
  -- | Creates an attributable element and ensures it has the given attribute
  createAttributed :: Attribute b -> PAttributable a b -> IO (a b)
  -- | Adds a new attribute to an attributable element and returns it
  addAttribute :: (AttribValue c) => Attribute c -> a b -> IO (Attribute c)
  -- | Gets the value of the attribute from an attributable element
  getAttribute :: (AttribValue c) => Attribute c -> a b -> IO c
  -- | Sets the value of the given attribute in an attributable element and
  --   returns the attributable such that it declares the new attribute
  setAttribute :: (AttribValue c) => Attribute c -> c -> a b -> IO (a c)

class (Attributable e a, AttribValue a, AttribValue b) => Extendable e a b where
  type Target e a b :: *
  extendWithAttrib :: AttribValue b => e a -> Attribute b -> IO (Target e a b)

attributeNull :: IO (ForeignPtr C'attribute)
attributeNull = newForeignPtr nullPtr (c'attribute_free nullPtr)

attributeAlloc :: IO (ForeignPtr C'attribute)
attributeAlloc = do
  ptr <- c'attribute_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'attribute_free ptr)
    else error "Memory allocation failed in attributeAlloc"

attributeCreate :: (AttribValue a, Key a ~ Int)  => Int -> a -> IO (Attribute a)
attributeCreate key value = attribCreate key value

attributeListNull :: IO (ForeignPtr C'attribute_list)
attributeListNull = newForeignPtr nullPtr (c'attribute_list_free nullPtr)

attributeListAlloc :: IO (ForeignPtr C'attribute_list)
attributeListAlloc = do
  ptr <- c'attribute_list_alloc
  if ptr /= nullPtr
    then newForeignPtr ptr (c'attribute_list_free ptr)
    else error "Memory allocation failed in attributeListAlloc"

attributeListCreate :: Int -> IO (ForeignPtr C'attribute_list)
attributeListCreate size = do
  fattriblist <- attributeListAlloc
  withForeignPtr fattriblist $ \pattriblist -> do
    r <- c'attribute_list_create pattriblist 2
    if r /= c'SUCCESS
      then error $ "Failed to create attribute_list with " ++ (show r)
      else return fattriblist

instance (AttribValue a, Eq a, Eq (Key a)) => Eq (Attribute a) where
  (==) a b
    | attribIsNull a || attribIsNull b = False
    | otherwise = (attribKey   a == attribKey b  ) && 
                  (attribValue a == attribValue b)

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

nodeFromListItem :: AttribValue a => Attribute a -> Ptr C'list_item
    -> IO (Node a)
nodeFromListItem attrib pitem = do
  item <- peek pitem
  createAttributed attrib $ castPtr $ c'list_item'data item

--nodeLinks :: Node -> [Link]
--nodeNeighbors :: Node -> [Node]

instance (AttribValue a) => Attributable Node a where
  type PAttributable Node a = Ptr C'node
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
  extendWithAttrib (node@(Node pnode (x,y) o s attrib1)) attrib2 = do
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
    nodea <- createEmpty $ c'link_head'origin a
    nodeb <- createEmpty $ c'link_head'origin b
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

-- | Creates an empty graph. In the underlying structure, memory will be
--   allocated for the given amount of nodes and links.
graphCreate :: (AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) =>
    Int -> Int -> Attribute a -> IO (Graph a)
graphCreate nodeSize linkSize attrib = do
  fgraph <- graphAlloc
  withForeignPtr fgraph $ \pgraph ->
    withForeignPtr (attribPtr attrib) $ \pattr -> do
      r <- c'graph_create pgraph
               (fromIntegral nodeSize)
               (fromIntegral linkSize)
               pattr
      if r /= c'SUCCESS
        then error $ "Failed to create graph with " ++ (show r)
        else graphFromPtr fgraph attrib

graphFromPtr :: AttribValue a => ForeignPtr C'graph -> Attribute a
    -> IO (Graph a)
graphFromPtr fgraph attrib =
  withForeignPtr fgraph $ \pgraph -> do
    nodes <- createList (p'graph'nodes pgraph) (nodeFromListItem attrib)
    links <- createList (p'graph'links pgraph) (linkFromListItem) -- key
    return $ Graph fgraph nodes links

-- | Creates a regular grid graph from an image. The step in pixels between 
--   grid rows and cols can be given, like also the neighborhood type and the
--   label for the attribute that will be used for storing the pixel values.
graphFromImage :: 
    (Num a, AttribValue a, PAttribValue a ~ ForeignPtr C'attribute) => 
    PixelImage -> Int -> Int -> Int -> Int -> GraphNeighborhood -> Attribute a
    -> IO (Graph a)
graphFromImage image offsetx offsety stepx stepy neighborhood attrib = do
  fgraph <- graphAlloc
  withForeignPtr fgraph $ \pgraph ->
    withForeignPtr (imagePtr image) $ \pimage ->
      withForeignPtr (attribPtr attrib) $ \pattrib -> do
        r <- c'graph_create_from_image
            pgraph
            pimage
            (fromIntegral offsetx)
            (fromIntegral offsety)
            (fromIntegral stepx)
            (fromIntegral stepy)
            (cNeighborhood neighborhood)
            pattrib
        if r /= c'SUCCESS
          then error $ "Failed to create graph from image with " ++ (show r)
          else graphFromPtr fgraph attrib
