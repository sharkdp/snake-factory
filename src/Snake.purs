module Snake where

import Prelude

import Data.List
import Data.Generic
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data PartColor = Red | Blue | Yellow

data Part = Part PartColor
data Snake = Snake (List Part)

derive instance genericPart :: Generic Part
derive instance genericPartColor :: Generic PartColor

instance eqPart :: Eq Part where eq = gEq
instance eqPartColor :: Eq PartColor where eq = gEq

instance arbitraryPart :: Arbitrary Part where
    arbitrary = do
        color <- elements Red [Blue, Yellow]
        return (Part color)

instance arbitrarySnake :: Arbitrary Snake where
    arbitrary = do
        len <- chooseInt 3 8
        ps <- listOf len arbitrary
        return (Snake ps)

arbitraryStriped :: Gen Snake
arbitraryStriped = do
    n <- chooseInt 2 5
    x <- (arbitrary :: Gen Part)
    y <- arbitrary
    return $ Snake $ concat $ (replicate n (x : y : Nil))
