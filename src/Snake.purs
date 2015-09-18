module Snake where

import Prelude

import Data.List
import Data.Generic

data PartColor = Red | Blue | Yellow

data Part = Part PartColor
type Snake = List Part

derive instance genericPartColor :: Generic PartColor

instance eqPartColor :: Eq PartColor where eq = gEq
