module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Either
import Data.List
import Data.Maybe

import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (randomSample')

import DOM

import Graphics.Canvas (getCanvasElementById, getContext2D, clearRect)
import Graphics.Drawing (render, translate)

import Graphics
import Language.Parsel.Interpreter
import Language.Parsel.Parser
import Snake

foreign import onRun :: forall eff. (String -> Eff (dom :: DOM | eff) Unit)
                     -> Eff (dom :: DOM | eff) Unit

foreign import printMessage :: forall eff. String -> Eff (dom :: DOM | eff) Unit

main = do
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas

    onRun $ \code -> do
        case (parseParsel code) of
            Left err -> do
                printMessage $ "Parse error: " ++ show err
            Right machine -> do
                printMessage $ show machine
                ssa <- (randomSample' 16 arbitrary)
                let ss = toList ssa
                print machine
                let ss' = runMachine machine ss
                clearRect ctx { x: 0.0, y: 0.0, w: 750.0, h: 400.0 }
                render ctx $ translate 10.0 10.0 $
                    snakes 0.0 ss <> snakes (13.0 * size) ss'
