module Game.Action where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

-- forall m. MonadRWS m =>
move :: RWS Environment [String] GameState Point
move = undefined

jump :: undefined
jump = undefined
