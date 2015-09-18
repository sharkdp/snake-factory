module Language.Parsel.Spec where

import Prelude
import Data.Generic

import Snake

data Predicate = LongerThan Int
               | ShorterThan Int
               | And Predicate Predicate
               | Or Predicate Predicate

data Transformer = Attach PartColor
                 | Tail
                 | FilterP PredicateP
                 | ComposeT Transformer Transformer

data PredicateP = HasColor PartColor

data Machine = Filter Predicate
             | Map Transformer
             | Compose Machine Machine

derive instance genericPredicate :: Generic Predicate
derive instance genericTransformer :: Generic Transformer
derive instance genericPredicateP :: Generic PredicateP
derive instance genericMachine :: Generic Machine

instance showMachine :: Show Machine where show = gShow
