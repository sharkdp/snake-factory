module Language.Parsel.Spec where

import Prelude
import Data.Generic

import Snake

data Machine = Filter Predicate
             | Map Transformer
             | MapIf Predicate Transformer
             | Compose Machine Machine

data Predicate = LongerThan Int
               | ShorterThan Int
               | Contains PartColor

data Transformer = Attach PartColor
                 | FilterP PredicateP
                 | Tail
                 | ComposeT Transformer Transformer

data PredicateP = HasColor PartColor
                | NotColor PartColor

derive instance genericPredicate :: Generic Predicate
derive instance genericTransformer :: Generic Transformer
derive instance genericPredicateP :: Generic PredicateP
derive instance genericMachine :: Generic Machine

instance showMachine :: Show Machine where show = gShow
instance eqMachine :: Eq Machine where eq = gEq
