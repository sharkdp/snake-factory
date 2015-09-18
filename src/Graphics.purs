module Graphics where

import Prelude

import Control.Monad.Eff

import Data.Maybe
import Data.List
import Data.Monoid

import Graphics.Drawing

import Snake

size :: Number
size = 15.0

color :: PartColor -> Color
color Red  =   rgb 255.0  57.0  95.0
color Blue =   rgb  31.0 136.0 178.0
color Yellow = rgb 255.0 246.0  82.0

part :: Part -> Drawing
part (Part c) =
    shade $ style $ rectangle 0.0 0.0 size size
      where style =    outlined (outlineColor black <> lineWidth 2.0)
                    <> filled (fillColor (color c))
            shade = shadow (shadowColor black <> shadowOffset 2.0 2.0 <> shadowBlur 10.0)

snake :: Snake -> Drawing
snake (Snake s) = go s 0.0
    where go Nil _              = mempty
          go (Cons p ps) offset = translate 0.0 offset (part p) <> go ps (offset + size)

snakes :: Number -> List Snake -> Drawing
snakes yoffset xs = go xs 0.0
    where go Nil _              = mempty
          go (Cons s ss) offset = translate offset yoffset (snake s) <> go ss (offset + 3.0 * size)
