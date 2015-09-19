module Language.Parsel.Interpreter
  ( runMachine
  ) where

import Prelude

import Data.Foldable
import Data.List

import Data.Maybe

import Language.Parsel.Spec
import Snake

runPredicate :: Predicate -> Snake -> Boolean
runPredicate (ShorterThan n) (Snake s) = length s < n
runPredicate (LongerThan n) (Snake s)  = length s > n
runPredicate (Contains c) (Snake s)    = (Part c) `elem` s

runPredicateP :: PredicateP -> Part -> Boolean
runPredicateP (HasColor c) (Part c') = c == c'
runPredicateP (NotColor c) (Part c') = c /= c'

runTransformer :: Transformer -> Snake -> Snake
runTransformer (Attach c) (Snake ps)  = Snake $ Cons (Part c) ps
runTransformer (Tail) (Snake ps)      = Snake $ fromMaybe Nil (tail ps)
runTransformer (FilterP p) (Snake ps) = Snake $ filter (runPredicateP p) ps
runTransformer (ComposeT t1 t2) s     = (runTransformer t1 >>> runTransformer t2) s

runMachine :: Machine -> List Snake -> List Snake
runMachine (Filter p)      = filter (runPredicate p)
runMachine (Map t)         = map (runTransformer t)
runMachine (MapIf p t)     = map (\s -> if runPredicate p s then runTransformer t s else s)
runMachine (Compose m1 m2) = runMachine m1 >>> runMachine m2
