{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

loadAssets :: IO Assets
-- loadBMP, readFile, etc.
loadAssets = do
    keyImg     <- loadBMP "./assets/graphics/food.bmp"
    -- doorImg    <- loadDoor -- I don't have the assets now
    -- grassImg   <- loadBMP "./assets/graphics/grassMid.bmp"
    baseImg    <- loadBMP "./assets/graphics/tile.bmp" -- "./assets/graphics/baseCenter.bmp"
    playerImgs <- loadPlayers
    return Sprites
        { _aPlayer = playerImgs
        , _aKey    = keyImg
        , _aDoor   = [keyImg, keyImg] -- doorImg
        , _aGrass  = baseImg -- grassImg
        , _aBase   = baseImg
        }

loadPlayers :: IO [Picture]
loadPlayers = do
    c1l1 <- loadBMP "./assets/graphics/characters/l/1.bmp"
    c1l2 <- loadBMP "./assets/graphics/characters/l/2.bmp"
    c1r1 <- loadBMP "./assets/graphics/characters/r/1.bmp"
    c1r2 <- loadBMP "./assets/graphics/characters/r/2.bmp"
    return
        [ c1l1, c1l1, c1l1, c1l2, c1l2, c1l2 -- facing left
        , c1r1, c1r1, c1r1, c1r2, c1r2, c1r2 -- facing right
        ]
    

loadDoor :: IO [Picture]
loadDoor = do
    dClose <- loadBMP "./assets/graphics/doorClosed.bmp"
    dOpen  <- loadBMP "./assets/graphics/doorOpened.bmp"
    return [dClose, dOpen]

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
