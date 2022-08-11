{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Game.Draw where

import Control.Lens
import Control.Monad.RWS
import Data.Maybe

import Game.Action
import Game.AssetManagement
import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State
import Game.Logic
import Game.Data.Enum

import Graphics.Gloss

renderGame :: RWST Environment [String] GameState IO Picture
renderGame = do
    env          <- ask
    level        <- use (gLevelState . lLevelCells)
    tiles        <- mapM drawTile level
    playerPos    <- use (gPlayerState . pPosition)
    playerSprite <- getPlayerSprite
    text         <- renderText
    background   <- renderBackground

    --Probably need to replace with renderLevel function?
    return . pictures $ 
        background ++ 
        uncurry translate playerPos playerSprite :
        tiles ++ 
        text
    

updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame sec = do
    gs <- get
    
    gDeltaSec .= sec -- it's okay to always set this into state
                     -- might need this for other screen states
                     -- value should be 1/FPS normally
    
    paused <- use gPaused
    
    case paused of
        True -> 
            return gs
        False -> do
            movePlayer
            incPlayerSprite
            
            keys <- incKeys
            gPlayerState . pCollectedKeys .= keys
            
            updatedLevel  <- removeItem
            gLevelState . lLevelCells .= updatedLevel
            
            door <- openDoor    
            gDoorOpen .= door
            
            nextState <- get
            return nextState



-- Helper Functions:
renderTile :: (MonadRWS Environment [String] GameState m) =>
    CellType -> m Picture
renderTile cellType = do
    env <- ask
    let baseImg  = view (eSprites . aBase ) env
        grassImg = view (eSprites . aGrass) env
        coinImg  = head $ view (eSprites . aCoin ) env
        keyImg   = view (eSprites . aKey  ) env
        doorImgs = view (eSprites . aDoor) env

    isDoorOpen <- use gDoorOpen

    doorTup <- getDoorSprite

    return $ case cellType of
        '*' -> baseImg 
        '^' -> grassImg
        'c' -> coinImg
        'k' -> fst keyImg
        't' -> fst doorTup
        'b' -> snd doorTup
        _   -> circle 0 -- should never reach here

drawTile :: (MonadRWS Environment [String] GameState m) =>
    Cell -> m Picture
drawTile (pos, celltYpe) = do
    tile <- renderTile celltYpe
    return . uncurry translate pos $ tile

renderText :: (MonadRWS Environment [String] GameState m) =>
    m [Picture]
renderText = do
    env          <- ask
    paused       <- use gPaused
    level        <- use (gLevelState . lLevelName)
    let continue  = view (eSprites . aTxtPause) env
    let title     = view (eSprites . aTxtTitle) env
    let enter     = view (eSprites . aTxtEnter) env
    let startText = [uncurry translate (0,200) title, uncurry translate (0,-200) enter] 

    case paused of
        True  -> case level of
                    LevelStart  -> return startText --Add credits screen?
                    _           -> return [continue]
        False -> case level of
                    LevelStart  -> return startText 
                    _           -> return []


renderBackground :: (MonadRWS Environment [String] GameState m) =>
    m [Picture]
renderBackground = do
    env      <- ask
    level   <- use (gLevelState . lLevelName)

    let lvlList = view (eSprites . aLvlNames) env
    let bgImgs = view (eSprites . aBgImg) env
    let zipLvls = zip lvlList bgImgs
    let imgToUse = lookup (show level) zipLvls

    case imgToUse of
        Nothing -> return []
        Just x  -> return [x]
    


