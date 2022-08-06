module Game.AssetManagement where

import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

loadAssets :: IO Assets
loadAssets = do
    keyImg     <- loadBMP "./assets/graphics/keyYellow.bmp"
    coinImg    <- loadBMP "./assets/graphics/coin.bmp"
    bg <- loadBMP "./assets/graphics/SKY_BG_1.bmp" 
    doorImg    <- loadDoor
    playerImgs <- loadPlayers
    baseImgs <- loadBaseTiles
    return Sprites
        { _aPlayer = playerImgs
        , _aKey    = keyImg
        , _aDoor   = doorImg
        , _aBaseTiles  = baseImgs
        , _aCoin   = coinImg
        }

loadPlayers :: IO [Picture]
loadPlayers = do
    left1 <- loadBMP "./assets/graphics/characters/left1.bmp"
    left2 <- loadBMP "./assets/graphics/characters/left2.bmp"
    left3 <- loadBMP "./assets/graphics/characters/left3.bmp"
    left4 <- loadBMP "./assets/graphics/characters/left4.bmp"
    right1 <- loadBMP "./assets/graphics/characters/right1.bmp"
    right2 <- loadBMP "./assets/graphics/characters/right2.bmp"
    right3 <- loadBMP "./assets/graphics/characters/right3.bmp"
    right4 <- loadBMP "./assets/graphics/characters/right4.bmp"
    --More
    return $ [left1,left2,left3,left4,right1,right2,right3,right4]

loadBaseTiles :: IO [Picture]
loadBaseTiles = do
    grassImg   <- loadBMP "./assets/graphics/grassMid.bmp"
    baseImg    <- loadBMP "./assets/graphics/baseCenter.bmp"
    return [grassImg, baseImg]

loadDoor :: IO [Picture]
loadDoor = do
    doorCMImg <- loadBMP "./assets/graphics/door_closedMid.bmp"
    doorCTImg <- loadBMP "./assets/graphics/door_closedTop.bmp"
    return [doorCMImg, doorCTImg]

loadLevels :: IO [String]
loadLevels = do
    level1 <- readFile "./assets/levels/level1.txt"
    level2 <- readFile "./assets/levels/level2.txt"
    level3 <- readFile "./assets/levels/level3.txt"    
    return $ [level1,level2,level3]

incPlayerSprite :: Float -> RWS Environment [String] GameState Picture
-- it takes sec :: Float, updates player sprite index (GameState),
-- returns current sprite :: Picture from the reader variable
incPlayerSprite sec = undefined

incDoorSprite :: RWS Environment [String] GameState Picture
-- increments player collected keys (GameState.PlayerState),
-- when collected keys == total keys then unlock door,
-- returns current door sprite (from Assets)
incDoorSprite = undefined
