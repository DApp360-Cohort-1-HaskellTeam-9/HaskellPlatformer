module Game.Logic where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

-- forall m. MonadRWS m =>
removeItem :: RWS Environment [String] GameState GameLevel
removeItem = undefined
{-
removeItem gs = do
    tileSize <- tileSize <$> ask
    let level = filter (\cell -> not (anyCollision tileSize (fst cell) (position gs) && snd cell == '%')) (currentLevel gs)
    return level
-}
updateSpeedY :: RWS Environment [String] GameState Float
updateSpeedY = undefined

updateSpeedX :: RWS Environment [String] GameState Float
updateSpeedX = undefined

playerCollision :: Cell -> RWS Environment [String] GameState (Maybe Point)
playerCollision = undefined

--Logic to check for keys and update door to door open sprite
openDoor :: RWS Environment [String] GameState GameLevel
openDoor = undefined

newLevel :: RWS Environment [String] GameState GameLevel
newLevel = undefined

