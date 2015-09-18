module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Either
import Data.List
import Data.Maybe

import Text.Parsing.StringParser (runParser)

import DOM

import Graphics.Canvas (getCanvasElementById, getContext2D, clearRect)
import Graphics.Drawing (render, translate)

import Graphics
import Language.Parsel.Interpreter
import Language.Parsel.Parser
import Snake

foreign import onClick :: forall eff. (String -> Eff (dom :: DOM | eff) Unit)
                       -> Eff (dom :: DOM | eff) Unit

main = do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    let ss =  (Part <$> (Red : Blue : Red : Nil))
            : (Part <$> (Red : Blue : Red : Blue : Red : Nil))
            : (Part <$> (Red : Red : Red : Blue : Red : Nil))
            : (Part <$> (Red : Blue : Blue : Blue : Red : Nil))
            : Nil

    onClick $ \code -> do
        case (runParser parseMachine code) of
            Left err -> do
                log $ "Parse error: " ++ show err
            Right machine -> do
                print machine
                let ss' = runMachine machine ss
                clearRect ctx { x: 0.0, y: 0.0, w: 300.0, h: 300.0 }
                render ctx $ translate 10.0 10.0 $
                    snakes 0.0 ss <> snakes (8.0 * size) ss'
