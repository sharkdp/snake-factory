module Language.Parsel.Spec where

import Prelude

import Data.Generic

data Predicate = LongerThan Int | ShorterThan Int

data Color = Red | Blue

data Transformer = Attach Color | Tail

data Machine = Filter Predicate
             | Map Transformer
             | Compose Machine Machine

derive instance genericPredicate :: Generic Predicate
derive instance genericColor :: Generic Color
derive instance genericTransformer :: Generic Transformer
derive instance genericMachine :: Generic Machine

instance showMachine :: Show Machine where show = gShow
