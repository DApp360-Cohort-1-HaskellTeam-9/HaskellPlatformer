module Game.Input where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss.Interface.IO.Game

handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys = undefined
