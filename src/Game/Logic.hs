module Game.Logic where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

-- forall m. MonadRWS m =>
removeItem :: RWS Environment [String] GameState GameLevel
removeItem = undefined

updateSpeedY :: RWS Environment [String] GameState Float
updateSpeedY = undefined

updateSpeedX :: RWS Environment [String] GameState Float
updateSpeedX = undefined

playerCollision :: Cell -> RWS Environment [String] GameState (Maybe Point)
playerCollision = undefined
