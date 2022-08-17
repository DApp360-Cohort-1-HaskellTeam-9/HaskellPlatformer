module Main where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.Data.Environment
import Game.Draw
import Game.Init
import Game.Input

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    let args = [] -- take arguments?
    
    -- init Environment
    env <- initEnv args
    (screenWidth, screenHeight) <- getScreenSize
    let windowWidth  = view eWindowWidth  env
        windowHeight = view eWindowHeight env
        x = (screenWidth  - windowWidth ) `div` 2
        y = (screenHeight - windowHeight) `div` 3
        -- window  = FullScreen
        window  = InWindow "Haskell Platformer" (windowWidth, windowHeight) (x, y)
        bgColor = black
        fps     = view eFPS env
    
    world <- runReaderT (initState args) env
    
    let draw world = do
            (scene, logMessages) <- evalRWST renderGame env world
            mapM_ putStrLn logMessages
            return scene
    
    let handleEvent event currState = do
            (nextState, logMessages) <- evalRWST (handleKeys event) env currState
            mapM_ putStrLn logMessages
            return nextState
    
    let step sec currState = do
            (nextState, logMessages) <- evalRWST (updateGame sec) env currState
            mapM_ putStrLn logMessages
            return nextState
    
    playIO window bgColor fps world draw handleEvent step
