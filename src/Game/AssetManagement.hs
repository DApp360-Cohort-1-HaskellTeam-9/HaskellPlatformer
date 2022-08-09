{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State
import Data.Maybe

import Graphics.Gloss

--loadImgs :: RWST Environment [String] GameState IO [Picture]

initAssets :: IO Assets
initAssets = do
    keyImg      <- loadBMP "./assets/graphics/items/key.bmp"
    coinImgs    <- loadCoin
    doorImgs    <- loadDoor
    bgImgs      <- loadBackgrounds
    playerImgs  <- loadPlayers
    baseImgs    <- loadBaseTiles

    return Sprites
        { _aPlayer = playerImgs
        , _aKey    = (keyImg, 'k')
        , _aDoor   = doorImgs
        , _aBase   = last $ baseImgs -- TODO: Is there a better function?
        , _aGrass  = head $ baseImgs -- TODO: Is there a better function?
        , _aCoin   = coinImgs
        , _aBgImg  = bgImgs
        }

rootDir :: String
rootDir = "./assets/graphics/"

loadPlayers :: IO [Picture]
loadPlayers = do
    let dir = rootDir ++ "characters/"
    let imgNames = map (\x -> "player" ++ x ++ "_35x45") ["Left1","Left2","Left3","Left4","Right1","Right2","Right3","Right4"]
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
    let imgNames = ["door_openMid", "door_openTop", "door_closedMid", "door_closedTop"]
    doorImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return doorImgs

--Add more backgrounds later
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
    m (Picture, Picture)
getDoorSprite = do
    env <- ask
    isDoorOpen <- use gDoorOpen
    let doorImgs = (view (eSprites . aDoor) env)
    let (doorTopImg,doorBottomImg) = 
            case isDoorOpen of
                True  -> case   (doorImgs ^? element 1, doorImgs ^? element 0) of
                                (Just x, Just y)  -> (Just x, Just y)
                                (_,_)             -> (Nothing, Nothing)       
                False -> case   (doorImgs ^? element 3, doorImgs ^? element 2) of
                                (Just x, Just y)  -> (Just x, Just y)     
                                (_,_)             -> (Nothing, Nothing)   
    return (fromJust $ doorTopImg, fromJust $ doorBottomImg)

