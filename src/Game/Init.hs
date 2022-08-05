module Game.Init where

import Control.Monad.Reader

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

initEnv :: [String] -> Environment
initEnv args = Environment {}

initState :: [String] -> Reader Environment GameState
-- may need to take values from Environment?
initState args = do
    let level = undefined -- init level
    return GameState {}
