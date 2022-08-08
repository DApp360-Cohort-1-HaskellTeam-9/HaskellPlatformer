{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

--loadImgs :: RWST Environment [String] GameState IO [Picture]

initAssets :: IO Assets
initAssets = do
    keyImg    <- loadBMP "./assets/graphics/items/key.bmp"
    baseImgs  <- loadBaseTiles
    coinImg   <- loadCoin
    doorImg   <- loadDoor
    bgImg     <- loadBackgrounds
    playerImg <- loadPlayers
    baseImgs  <- loadBaseTiles

    return Sprites
        { _aPlayer = playerImg
        , _aKey    = keyImg
        , _aDoor   = doorImg
        , _aBase   = last $ baseImgs -- TODO: Is there a better function?
        , _aGrass  = head $ baseImgs -- TODO: Is there a better function?
        , _aCoin   = coinImg
        , _aBgImg  = bgImg
        }

rootDir :: String
rootDir = "./assets/graphics/"

loadPlayers :: IO [Picture]
loadPlayers = do
    let dir = rootDir ++ "characters/"
    let imgNames = ["playerLeft1","playerLeft2","playerLeft3","playerLeft4","playerRight1","playerRight2","playerRight3","playerRight4"]
    playerImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames -- Could also sequence to flip type
    return playerImgs

loadCoin :: IO [Picture]
loadCoin = do
    let dir = rootDir ++ "items/"
    let imgNames = ["coin"]
    coinImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return coinImgs

loadBaseTiles :: IO [Picture]
loadBaseTiles = do
    let dir = rootDir ++ "base/"
    let imgNames = ["grass", "base"]
    baseImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return baseImgs

loadDoor :: IO [Picture]
loadDoor = do
    let dir = rootDir ++ "door/"
    let imgNames = ["door_closedMid", "door_closedTop", "door_openMid", "door_openTop"]
    doorImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return doorImgs

loadBackgrounds :: IO [Picture]
loadBackgrounds = do
    let dir = rootDir ++ "backgrounds/"
    let imgNames = ["skyBackground"]
    bgImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return bgImgs

loadLevels :: IO [String]
loadLevels = do
    let dir = "assets/levels/"
    let lvlNames = ["level1", "level2", "level3"]
    levels <- mapM (readFile . (\n -> dir ++ n ++ ".txt")) lvlNames
    return levels

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
