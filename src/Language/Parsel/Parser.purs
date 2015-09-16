module Language.Parsel.Parser
  ( parseMachine
  ) where

import Prelude

import qualified Data.Char as C
import Data.Either
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.List (List(..), toList)
import Data.Int (fromString)
import Data.Foldable

import Control.Alt
import Control.Apply ((*>))

import Control.Monad.Eff

import Text.Parsing.StringParser
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Combinators

import Language.Parsel.Spec

sepWhite :: Parser Unit
sepWhite = many1 (string " ") *> return unit

skipWhite :: Parser Unit
skipWhite = many (string " ") *> return unit

bracketed :: forall a. Parser a -> Parser a
bracketed = between (string "(" *> skipWhite)
                    (string ")" *> skipWhite)

integer :: Parser Int
integer = do
    digits <- many1 anyDigit
    let str = foldMap C.toString digits
    return $ fromJust (fromString str)

parsePredicate :: Parser Predicate
parsePredicate =     (try $ bracketed $ string ">" *> skipWhite *> (LongerThan <$> integer))
                 <|> (try $ bracketed $ string "<" *> skipWhite *> (ShorterThan <$> integer))
                 <?> "Expected valid predicate"

color :: Parser Color
color = (string "red" *> return Red) <|> (string "blue" *> return Blue)

parseTransformer :: Parser Transformer
parseTransformer =     (bracketed $ (string "attach" *> sepWhite *> (Attach <$> color)))
                   <|> (string "tail" *> return Tail)
                   <?> "Expected valid transformer"

parseFilter :: Parser Machine
parseFilter = string "filter" *> sepWhite *> (Filter <$> parsePredicate)

parseMap :: Parser Machine
parseMap = string "map" *> sepWhite *> (Map <$> parseTransformer)

parseMachineExpr :: Parser Machine
parseMachineExpr =     parseFilter
                   <|> parseMap
                   <?> "Expected valid Machine"

parseMachine :: Parser Machine
parseMachine = do
    ex1 <- parseMachineExpr
    rest <- optionMaybe (skipWhite *> string ">>>" *> skipWhite *> parseMachine)
    case rest of
         Just machine -> return $ Compose ex1 machine
         Nothing -> return ex1
