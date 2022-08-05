module Game.Init where

import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Environment
import Game.Data.State

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- loadAssets
    return Environment
        { _eSprites = assets
        }

initState :: [String] -> Reader Environment GameState
-- may need to take values from Environment?
initState args = do
    let level = undefined -- init level
    return GameState {}
