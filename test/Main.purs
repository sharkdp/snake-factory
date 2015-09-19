module Test.Main where

import Prelude

import Data.Either
import Data.Foldable

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Exception

import Language.Parsel.Interpreter
import Language.Parsel.Parser
import Language.Parsel.Spec
import Snake

fail :: forall eff. String -> Eff (err :: EXCEPTION | eff) Unit
fail msg = throwException $ error msg


shouldParseAs :: forall eff. Machine -> String -> Eff (err :: EXCEPTION | eff) Unit
shouldParseAs expected input =
    case parseParsel input of
        Left err -> fail $ "Parse error for input '" ++ input ++ "': " ++ show err
        Right output ->
            unless (output == expected) $ do
                fail $ "Unexpected result:\n" ++
                       "Input:    '" ++ input ++ "'\n" ++
                       "Output:   " ++ show output ++ "\n" ++
                       "Expected: " ++ show expected ++ "\n"

allParseAs :: forall eff. Machine -> Array String -> Eff (err :: EXCEPTION | eff) Unit
allParseAs expected = traverse_ (shouldParseAs expected)

shouldFail :: forall eff. String -> Eff (err :: EXCEPTION | eff) Unit
shouldFail input = do
    case (parseParsel input) of
         Left _ -> return unit
         Right output -> fail $ "Should throw a parse error: '" ++ input ++ "'"

main = do
    allParseAs (Map Tail)
        [ "map (tail)"
        , "  map    ( tail   )"
        , "map ((((tail))))"
        , "map ((((tail))))"
        ]

    shouldFail "maptail"
    shouldFail "tail"

    allParseAs (Filter (LongerThan 42))
        [ "filter (> 42)"
        , "filter (>42)"
        , "filter   (  >   42  )"
        , "filter (((> 42)))"
        ]

    shouldFail "filter > 42"

    allParseAs (Map (FilterP (HasColor Red)))
        [ "map (filter (= red))"
        , "map   ( filter (=r))"
        ]

    shouldFail "map (filter =r)"
    shouldFail "map (filter (= magenta))"

    allParseAs (Map (ComposeT Tail (FilterP (NotColor Yellow))))
        [ "map (tail >>> filter (~ yellow))"
        ]

    allParseAs (Compose (Map Tail) (Map Tail))
        [ "map (tail) >>> map (tail)"
        , "  map (tail)>>>map (tail)  "
        ]

    allParseAs (Map (ComposeT Tail (Attach Red)))
        [ "map (tail >>> (attach red))"
        ]
