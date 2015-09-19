module Language.Parsel.Parser
  ( parseParsel
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
import Control.Apply

import Control.Monad.Eff

import Text.Parsing.StringParser
import Text.Parsing.StringParser.String
import Text.Parsing.StringParser.Combinators

import Language.Parsel.Spec

import Snake

whitespace :: Parser String
whitespace = string " " <|> string "\n" <|> string "\t"

sepWhite :: Parser Unit
sepWhite = many1 whitespace *> return unit

skipWhite :: Parser Unit
skipWhite = many whitespace *> return unit

enclosed :: forall a. Parser a -> Parser a
enclosed = between (string "(" *> skipWhite)
                   (skipWhite *> string ")")

inParens :: forall a. Parser a -> Parser a
inParens parser = fix \p -> (parser <|> enclosed p)

inParens1 :: forall a. Parser a -> Parser a
inParens1 = enclosed <<< inParens

integer :: Parser Int
integer = do
    digits <- many1 anyDigit
    let str = foldMap C.toString digits
    return $ fromJust (fromString str)

color :: Parser PartColor
color =     (string "r" *> optional (string "ed") *> return Red)
        <|> (string "b" *> optional (string "lue") *> return Blue)
        <|> (string "y" *> optional (string "ellow") *> return Yellow)
        <?> "Expected valid color name"

comp :: forall a. (a -> a -> a) -> Parser (a -> a -> a)
comp fn = try $ skipWhite *> string ">>>" *> skipWhite *> return fn

parsePredicate :: Parser Predicate
parsePredicate =     (try $ enclosed $ string ">" *> skipWhite *> (LongerThan <$> integer))
                 <|> (try $ enclosed $ string "<" *> skipWhite *> (ShorterThan <$> integer))
                 <|> (try $ enclosed $ string "contains" *> sepWhite *> (Contains <$> color))
                 <?> "Expected valid predicate"

parsePredicateP :: Parser PredicateP
parsePredicateP =     string "=" *> skipWhite *> (HasColor <$> color)
                  <|> string "~" *> skipWhite *> (NotColor <$> color)
                  <?> "Expected valid part predicate"

parseTransformerExpr :: Parser Transformer
parseTransformerExpr =     (string "attach" *> sepWhite *> (Attach <$> color))
                       <|> (string "filter" *> sepWhite *> (FilterP <$> (inParens1 parsePredicateP)))
                       <|> (string "tail" *> return Tail)
                       <?> "Expected valid transformer"

parseTransformer :: Parser Transformer
parseTransformer = inParens1 (chainl1 (inParens parseTransformerExpr) (comp ComposeT))

parseFilter :: Parser Machine
parseFilter = string "filter" *> sepWhite *> (Filter <$> (inParens parsePredicate))

parseMap :: Parser Machine
parseMap = string "map" *> sepWhite *> (Map <$> parseTransformer)

parseMapIf :: Parser Machine
parseMapIf = string "mapIf" *> sepWhite *> (MapIf <$> parsePredicate <*> (sepWhite *> parseTransformer))

parseMachineExpr :: Parser Machine
parseMachineExpr =     parseFilter
                   <|> try parseMap
                   <|> parseMapIf
                   <?> "Expected valid Machine"

parseMachine :: Parser Machine
parseMachine = skipWhite *> chainl1 parseMachineExpr (comp Compose) <* skipWhite <* eof

parseParsel :: String -> Either ParseError Machine
parseParsel = runParser parseMachine
