module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Either

import Text.Parsing.StringParser (runParser)

import Language.Parsel.Parser

import DOM

foreign import clickHandler :: forall eff. (String -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit

main = do
    clickHandler $ \code -> do
        let res = runParser parseMachine code
        case res of
            Left err -> do
                log $ "Parse error: " ++ show err
            Right machine -> do
                print machine

