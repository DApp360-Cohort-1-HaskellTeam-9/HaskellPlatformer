module Game.Draw where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

renderGame :: RWST Environment [String] GameState IO Picture
renderGame = undefined

updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame sec = undefined

-- Helper Functions:
renderTile :: undefined
renderTile = undefined

checkImg :: undefined
checkImg = undefined
