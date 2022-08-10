module Main where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.Data.Environment
import Game.Draw
import Game.Init
import Game.Input

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    let args = [] -- take arguments?
    
    -- init Environment
    env <- initEnv args
    let windowWidth = view eWindowWidth env
    let windowHeight = view eWindowHeight env
    let window  = InWindow "Haskell Platformer" (windowWidth, windowHeight) (0, 0)
    let bgColor = black
    let fps     = view eFPS env
    
    world <- runReaderT (initState args) env
    
    let draw world = do
            (scene, logMessages) <- evalRWST renderGame env world
            -- do something with logMessages if needed
            return scene
    
    let handleEvent event currState = do
            (nextState, logMessages) <- evalRWST (handleKeys event) env currState
            -- do something with logMessages if needed
            return nextState
    
    let step sec currState = do
            (nextState, logMessages) <- evalRWST (updateGame sec) env currState
            -- do something with logMessages if needed
            return nextState
    
    playIO window bgColor fps world draw handleEvent step
