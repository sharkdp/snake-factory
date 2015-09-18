module Language.Parsel.Interpreter
  ( runMachine
  ) where

import Prelude

import Data.List

import Data.Maybe

import Language.Parsel.Spec
import Snake

runPredicate :: Predicate -> Snake -> Boolean
runPredicate (ShorterThan n) s = length s < n
runPredicate (LongerThan n) s  = length s > n

runPredicateP :: PredicateP -> Part -> Boolean
runPredicateP (HasColor c) (Part c') = c == c'

runTransformer :: Transformer -> Snake -> Snake
runTransformer (Attach c) s  = Cons (Part c) s
runTransformer (Tail) s      = fromMaybe Nil (tail s)
runTransformer (FilterP p) s = filter (runPredicateP p) s

runMachine :: Machine -> List Snake -> List Snake
runMachine (Filter p)      = filter (runPredicate p)
runMachine (Map t)         = map (runTransformer t)
runMachine (Compose m1 m2) = runMachine m1 >>> runMachine m2
