{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

loadAssets :: IO Assets
loadAssets = do
    keyImg    <- loadBMP "./assets/graphics/keyYellow.bmp"
    coinImg   <- loadBMP "./assets/graphics/coin.bmp"
    doorImg   <- loadDoor
    baseImg   <- loadBMP "./assets/graphics/baseCenter.bmp"
    grassImg  <- loadBMP "./assets/graphics/grassMid.bmp"
    bgImg     <- loadBMP "./assets/graphics/SKY_BG_1.bmp"
    playerImg <- loadPlayers
    baseImgs <- loadBaseTiles
    return Sprites
        { _aPlayer = playerImg
        , _aKey    = keyImg
        , _aDoor   = doorImg
        , _aBase   = baseImg
        , _aGrass  = grassImg
        , _aCoin   = coinImg
        , _aBgImg  = bgImg
        }

--TEMPORARY!!!! just to show rendered game for now.
loadImgs :: RWST Environment [String] GameState IO [Picture]
loadImgs = do
    coinImg    <- lift $ loadBMP "./assets/graphics/coin.bmp"
    grassImg   <- lift $ loadBMP "./assets/graphics/grassMid.bmp"
    baseImg    <- lift $ loadBMP "./assets/graphics/baseCenter.bmp"
    keyImg     <- lift $ loadBMP "./assets/graphics/keyYellow.bmp"
    doorCMImg <- lift $ loadBMP "./assets/graphics/door_closedMid.bmp"
    doorCTImg <- lift $ loadBMP "./assets/graphics/door_closedTop.bmp"
    bg <- lift $ loadBMP "./assets/graphics/SKY_BG_1.bmp" 
    return $ [baseImg,grassImg,coinImg, keyImg, doorCTImg, doorCMImg, bg]

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

getPlayerSprite :: (MonadRWS Environment [String] GameState m) => 
    m Picture
getPlayerSprite = do
    env <- ask
    
    let playerSprites = view (eSprites . aPlayer) env
    playerSpriteIndex <- use $ gPlayerState . pSpriteIndex
    let playerSpriteI = truncate playerSpriteIndex `mod` length playerSprites 
    
    let for 0 (p:ps) = p
        for i (p:ps) = for (i - 1) ps
    return $ for playerSpriteI playerSprites

getDoorSprite :: (MonadRWS Environment [String] GameState m) =>
    m Picture
getDoorSprite = do
    env <- ask
    let [doorClosed, doorOpened] = view (eSprites . aDoor) env
    isOpen <- use gDoorOpen
    if isOpen
        then return doorOpened
        else return doorClosed
