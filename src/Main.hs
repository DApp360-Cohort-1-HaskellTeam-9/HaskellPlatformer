module Main where

import Control.Monad.Reader
import Control.Monad.RWS

import Game.Draw
import Game.Init
import Game.Input

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    let args = undefined -- take arguments?
    
    -- init Environment
    let env = initEnv args
    
    -- init Assets
    
    let window  = undefined
    let bgColor = undefined
    let fps     = undefined
    
    let world = runReader (initState args) env
    
    let draw currState = do
            (pic, logMessages) <- evalRWST renderGame env currState
            -- do something with logMessages if needed
            return pic
    
    let handleEvent event currState = do
            (nextState, logMessages) <- evalRWST (handleKeys event) env currState
            -- do something with logMessages if needed
            return nextState
    
    let step sec currState = do
            (nextState, logMessages) <- evalRWST (updateGame sec) env currState
            -- do something with logMessages if needed
            return nextState
    
    playIO window bgColor fps world draw handleEvent step
