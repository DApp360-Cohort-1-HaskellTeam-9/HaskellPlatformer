module Game.AssetManagement where

import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

loadAssets :: Assets
-- loadBMP, readFile, etc.
loadAssets = undefined

incPlayerSprite :: Float -> RWS Environment [String] GameState Picture
-- it takes sec :: Float, updates player sprite index (GameState),
-- returns current sprite :: Picture from the reader variable
incPlayerSprite sec = undefined

incDoorSprite :: RWS Environment [String] GameState Picture
-- increments player collected keys (GameState.PlayerState),
-- when collected keys == total keys then unlock door,
-- returns current door sprite (from Assets)
incDoorSprite = undefined
