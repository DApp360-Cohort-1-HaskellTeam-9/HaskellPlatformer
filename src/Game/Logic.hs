{-# LANGUAGE FlexibleContexts #-}

module Game.Logic where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Alias
import Game.Data.Asset
import Game.AssetManagement
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

removeItem :: (PureRWS m) => m GameLevel
removeItem = do
    env <- ask
    let tileSize = view eTileSize env
    let itemTiles = concat [getCoinCellType, getKeyCellType]
    
    currentLv <- use gCurrentLevel
    playerPos <- use (gPlayerState . pPosition)
    
    return $ filter
        (\ (cell, cellType) -> not ((cellType `elem` itemTiles) 
        && isHit cell playerPos tileSize))
        currentLv
    

collideWith :: (PureRWS m) => [CellType] -> Point -> m (Maybe Cell)
collideWith colliders point = do
    env <- ask
    let tileSize = view eTileSize env

    level <- use gCurrentLevel
    return $ foldr (\ (cell, cellType) next ->
        if cellType `elem` colliders && isHit point cell tileSize
            then Just (cell, cellType)
            else next)
        Nothing
        level
    

isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize =
    x1            < x2 + tileSize &&
    x1 + tileSize > x2            &&
    y1            < y2 + tileSize &&
    y1 + tileSize > y2

openDoor :: RWSIO Bool
openDoor = do
    gs <- get
    collectedKeys   <- use (gPlayerState . pCollectedKeys)
    totalKeys       <- use gTotalKeys
    currentLevel    <- use gCurrentLevel

    paused <- use gPaused
    
    if collectedKeys == totalKeys
        then do
            isDoorOpen <- use gDoorOpen
            --unless isDoorOpen $ playSound DoorOpen
            return True
        else do
            return False
        
    

-- incCoin :: RWST Environment [String] GameState IO Int
-- incCoin = do
--     env       <- ask
--     playerPos <- use (gPlayerState . pPosition)
--     -- collectedKeys <- use (gPlayerState . pCollectedKeys)
    
--     let coinCell = getCoinCellType
--     coinFound <- collideWith coinCell playerPos
    
--     case coinFound of
--         Nothing -> do
--             return 0
--         Just _  -> do
--             playSound Coin
--             return 1
        
    

incKeys :: (PureRWS m) => m Int
incKeys = do
    env           <- ask
    playerPos     <- use (gPlayerState . pPosition     )
    collectedKeys <- use (gPlayerState . pCollectedKeys)
    
    let keyCell = getKeyCellType -- view (eSprites . aKey) env
    keyFound <- collideWith keyCell playerPos
    
    case keyFound of
        Nothing -> do
            return collectedKeys
        Just _  -> do
            -- playSound Key
            return . succ $ collectedKeys
        
    
