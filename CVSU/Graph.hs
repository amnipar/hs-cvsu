module CVSU.Graph
(

) where

import CVSU.Bindings.Graph

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Concurrent

data Attribute a =
  NoSuchAttribute |
  Attribute
  { attributePtr :: !(ForeignPtr C'attribute)
    attributeId :: Int
    attributeValue :: a
  }

instance (Eq a) => Eq (Attribute a) where
  a == b
  | a == NoSuchAttribute || b == NoSuchAttribute = False
  | otherwise =
    (attributeId a == attributeId b) && (attributeValue a == attributeValue b)

-- | In order to be usable as attributes, a type must provide conversions from
-- and to typed pointers.
class Attributable a where
  convertTo :: C'typed_pointer -> IO a
  convertFrom :: a -> IO C'typed_pointer

-- | Creates an attribute label. This is used for declaring the typeclass of the
-- attributes for use in functions.
attributeLabel :: Int -> a -> Attribute a
attributeLabel attrId attrVal = Attribute nullPtr attrId attrVal

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

data Node =
  Node
  { nodePtr :: !(ForeignPtr C'node)
  , nodePosition :: (Double, Double)
  , nodeOrientation :: Double
  , nodeScale :: Int
  , nodeAttributes :: [Attribute]
  , nodeLinks :: [Link]
  } deriving Eq

nodeNeighbors :: Node -> [Node]

data Link =
  Link
  { linkPtr :: !(ForeignPtr C'link)
  , linkHead :: !(ForeignPtr C'link_head)
  , linkWeight :: Double
  , linkAttributes :: [Attribute]
  } deriving Eq

linkGetOrigin :: Link -> Node

linkGetOther :: Link -> Node

data Graph =
  Graph
  { graphPtr :: !(ForeignPtr C'graph
    nodes :: [Node]
    links :: [Link]
  } deriving Eq

data GraphNeighborhood =
  Neighborhood4 |
  Neighborhood8

-- | Creates an empty graph. In the underlying structure, memory will be
-- allocated for the given amount of nodes and links.
graphCreate :: Int -> Int -> Graph
graphCreate nodeSize linkSize =

-- | Creates a regular grid graph from an image. The step in pixels between grid
-- rows and cols can be given, like also the neighborhood type and the label for
-- the attribute that will be used for storing the pixel values.
graphFromImage :: PixelImage -> Int -> GraphNeighborhood -> Attribute a -> Graph
graphFromImage image step neighborhood attrLabel =
