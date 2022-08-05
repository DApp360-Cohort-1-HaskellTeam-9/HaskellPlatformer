module Game.AssetManagement where

import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

loadAssets :: IO Assets
-- loadBMP, readFile, etc.
loadAssets = do
    keyImg     <- loadBMP "./assets/graphics/key.bmp"
    doorImg    <- loadDoor
    grassImg   <- loadBMP "./assets/graphics/grassMid.bmp"
    baseImg    <- loadBMP "./assets/graphics/baseCenter.bmp"
    playerImgs <- loadPlayers
    return Sprites
        { _aPlayer = playerImgs
        , _aKey    = keyImg
        , _aDoor   = doorImg
        , _aGrass  = grassImg
        , _aBase   = baseImg
        }

loadPlayers :: IO [Picture]
loadPlayers = do
    c1l1 <- loadBMP "./assets/graphics/characters/l/1.bmp"
    c1l2 <- loadBMP "./assets/graphics/characters/l/1.bmp"
    c1r1 <- loadBMP "./assets/graphics/characters/l/1.bmp"
    c1r2 <- loadBMP "./assets/graphics/characters/l/1.bmp"
    return [c1l1, c1l2, c1r1, c1r2]

loadDoor :: IO [Picture]
loadDoor = do
    dClose <- loadBMP "./assets/graphics/doorClosed.bmp"
    dOpen  <- loadBMP "./assets/graphics/doorOpened.bmp"
    return [dClose, dOpen]

incPlayerSprite :: Float -> RWS Environment [String] GameState Picture
-- it takes sec :: Float, updates player sprite index (GameState),
-- returns current sprite :: Picture from the reader variable
incPlayerSprite sec = undefined

incDoorSprite :: RWS Environment [String] GameState Picture
-- increments player collected keys (GameState.PlayerState),
-- when collected keys == total keys then unlock door,
-- returns current door sprite (from Assets)
incDoorSprite = undefined
