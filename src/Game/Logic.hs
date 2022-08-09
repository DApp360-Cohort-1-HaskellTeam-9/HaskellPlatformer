{-# LANGUAGE FlexibleContexts #-}

module Game.Logic where

import Control.Lens
import Control.Monad.RWS
import Control.Monad.Extra

import Game.Data.Environment
import Game.Data.State
import Game.Data.Asset

import Graphics.Gloss

removeItem :: (MonadRWS Environment [String] GameState m) =>
    m GameLevel
removeItem = do
    env <- ask
    let tileSize = view eTileSize env
    let itemTiles = view eItemTiles env
    
    currentLv <- use gCurrentLevel
    playerPos <- use (gPlayerState . pPosition)
    
    return $ filter
        (\ (cell, cellType) -> not ((cellType `elem` itemTiles) 
        && isHit cell playerPos tileSize))
        currentLv
    

updateSpeedY :: (MonadRWS Environment [String] GameState m) =>
    m Float
updateSpeedY = do
    env <- ask
    let tileSize = view eTileSize env
    let itemTiles = view eItemTiles env
    let baseTiles = view eBaseTiles env

    delta <- use gDeltaSec
    
    (posX, posY) <- use (gPlayerState . pPosition )
    (spdX, spdY) <- use (gPlayerState . pSpeed    )
    (dirX, dirY) <- use (gPlayerState . pDirection)

    collidedTile <- mapM (\x -> isCollision (posX, posY + spdY) x) baseTiles
    let hasCollided = any (==True) collidedTile

    return $ if hasCollided
        then negate . abs $ spdY -- only negate upwards movement
        else max (-15) $ spdY - 0.5 -- TODO: Don't use constants!



updateSpeedX :: (MonadRWS Environment [String] GameState m) =>
    m Float
updateSpeedX = do
    (dirX, dirY) <- use (gPlayerState . pDirection)
    (spdX, spdY) <- use (gPlayerState . pSpeed    )
    return $ case dirX of
        0 -> max 0.0 $ spdX - 0.5
        _ -> min 7.5 $ spdX + 0.5
    

playerCollision :: (MonadRWS Environment [String] GameState m) =>
    Point -> m (Maybe Point)
playerCollision pnt = do
    env <- ask
    let tileSize = view eTileSize env
    
    level <- use gCurrentLevel
    return . foldr (\ (cell, cellType) next ->
        if (cellType == '*' || cellType == '^') &&
            isHit pnt cell tileSize
            then Just cell
            else next) Nothing $ level
        
-- TODO: Do we really need 3 different functions to check for collision?
isCollision :: (MonadRWS Environment [String] GameState m) =>
    Point -> CellType -> m Bool
isCollision pnt checkType = do
    env <- ask
    let tileSize = view eTileSize env   
    level <- use gCurrentLevel
    return $ any
        (\ (cell, cellType) -> cellType == checkType && isHit pnt cell tileSize)
        level


isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize = 
    (abs (x1-x2) < tileSize && 
    abs (y1-y2) < tileSize)


openDoor :: (MonadRWS Environment [String] GameState m) =>
    m Bool
openDoor = do
    gs <- get
    collectedKeys   <- use (gPlayerState . pCollectedKeys)
    totalKeys       <- use gTotalKeys
    currentLevel    <- use gCurrentLevel

    paused <- use gPaused

    return $
        case compare collectedKeys totalKeys of
            GT -> False
            LT -> False
            EQ -> True

incKeys :: (MonadRWS Environment [String] GameState m) => 
    m Int
incKeys = do
    env             <- ask
    playerPos       <- use (gPlayerState . pPosition)
    collectedKeys   <- use (gPlayerState . pCollectedKeys)
    let keyCell = view (eSprites . aKey) env
    keyFound <- isCollision playerPos (snd keyCell) 

    return $
        case keyFound of
            True  -> collectedKeys + 1
            False -> collectedKeys

