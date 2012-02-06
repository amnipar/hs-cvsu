
module Graphical
( Graphical(..)
, Colorable(..)
, drawWithColor
) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

class Graphical drawable where
  draw :: drawable -> Picture

class Colorable c where
  toColor :: c -> Color

instance Graphical Int where
  draw a = text (show a)

instance Graphical Char where
  draw a = text [a]

instance Graphical Double where
  draw a = text $ show a

instance (Show a, Graphical a) => Graphical [a] where
  draw a = text $ show a

drawWithColor :: (Graphical a) => (Color, a) -> Picture
drawWithColor (c,a) = color c $ draw a
