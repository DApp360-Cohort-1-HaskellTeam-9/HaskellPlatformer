{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Asset
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Data.Maybe

import Graphics.Gloss

--loadImgs :: RWST Environment [String] GameState IO [Picture]

initAssets :: IO Assets
initAssets = do
    keyImg      <- loadBMP "./assets/graphics/items/key.bmp"
    txtCont     <- loadBMP "./assets/graphics/text/continue.bmp" 
    coinImgs    <- loadCoin
    doorImgs    <- loadDoor
    bgImgs      <- loadBackgrounds
    playerImgs  <- loadPlayers
    baseImgs    <- loadBaseTiles
    lvlList     <- loadLevels

    return Sprites
        { _aPlayer  = playerImgs
        , _aKey     = (keyImg, 'k')
        , _aDoor    = doorImgs
        , _aBase    = last $ baseImgs -- TODO: Is there a better function?
        , _aGrass   = head $ baseImgs -- TODO: Is there a better function?
        , _aCoin    = coinImgs
        , _aBgImg   = bgImgs
        , _aTxtCont = txtCont
        , _aLevels  = lvlList
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
    let imgNames = ["skyBackground", "mountainBackground"]
    bgImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return bgImgs

loadLevels :: IO [String]
loadLevels = do
    let dir = "assets/levels/"
    let lvlNames = ["level1", "level2", "level3"]
    levels <- mapM (readFile . (\n -> dir ++ n ++ ".txt")) lvlNames
    return levels

incPlayerSprite :: (MonadRWS Environment [String] GameState m) =>
    m ()
incPlayerSprite = do
    movement <- use (gPlayerState . pMovement)
    case movement of
        MoveStop -> return ()
        _        -> do
            delta <- use gDeltaSec
            gPlayerState . pSpriteIndex %= (+delta*10)
        
    

getPlayerSprite :: (MonadRWS Environment [String] GameState m) => 
    m Picture
getPlayerSprite = do
    env <- ask
    
    let playerSprites    = view (eSprites . aPlayer) env
    let (lFaces, rFaces) = splitAt 4 playerSprites
    
    spriteIndex <- use (gPlayerState . pSpriteIndex)
    let i = truncate spriteIndex `mod` 4
    
    face <- use (gPlayerState . pHeading)
    return $ case face of
        FaceRight -> rFaces !! i
        FaceLeft  -> lFaces !! i
    

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

getCollidables :: [CellType] -- this is a list of collidables cell types
getCollidables = "*^" -- open to suggestions to improve this function :)

getCoinCellType :: [CellType]
getCoinCellType = "c"

getKeyCellType :: [CellType]
getKeyCellType = "k"

getDoorCellType :: [CellType]
getDoorCellType = "tb"

type Bounciness = Float
type BounceStop = Float
getBounciness :: CellType -> (Bounciness, BounceStop)
getBounciness c = case c of
    '^' -> (0.50, 0.05)
    '*' -> (0.25, 0.05)
    _   -> (0.00, 0.00)
