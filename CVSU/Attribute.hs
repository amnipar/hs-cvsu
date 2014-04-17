{-#LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts,
            UndecidableInstances #-}
module CVSU.Attribute
( AttribValue(..)
, Attributable(..)
, Extendable(..)
, Attribute(..)
, attributeAlloc
, attributeNull
, attributeCreate
, attributePair
, fstAttribute
, sndAttribute
) where

-- , FlexibleInstances, MultiParamTypeClasses, OverlappingInstances,
-- ScopedTypeVariables

import CVSU.Bindings.Types
import CVSU.Bindings.TypedPointer
import CVSU.Bindings.Attribute

import CVSU.Types
import CVSU.TypedPointer

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Concurrent
import System.IO.Unsafe

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

attributePair :: (AttribValue a, AttribValue b) =>
    Attribute a -> Attribute b -> IO (Attribute (a,b))
attributePair a b =
  attribCreate (attribKey a, attribKey b) (attribValue a, attribValue b)

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
  --createEmpty :: PAttributable a () -> IO (a ())
  -- | Creates an attributable element and ensures it has the given attribute
  createAttributed :: Attribute b -> PAttributable a b -> IO (a b)
  -- | Adds a new attribute to an attributable element and returns it
  addAttribute :: (AttribValue c) => Attribute c -> a b -> IO (Attribute c)
  -- | Gets the value of the attribute from an attributable element
  getAttribute :: (AttribValue c) => Attribute c -> a b -> IO c
  -- | Sets the value of the given attribute in an attributable element and
  --   returns the attributable such that it declares the new attribute
  setAttribute :: (AttribValue c) => Attribute c -> c -> a b -> IO (a c)
  --getValue :: (AttribValue c) => Attribute c -> a b -> IO (c)

class (Attributable e a, AttribValue a, AttribValue b) => Extendable e a b where
  type Target e a b :: *
  extendWithAttrib :: AttribValue b => Attribute b -> e a -> IO (Target e a b)

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
    r <- c'attribute_list_create pattriblist (fromIntegral size)
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