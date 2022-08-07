{-# LANGUAGE FlexibleContexts #-}

module Game.Logic where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

removeItem :: (MonadRWS Environment [String] GameState m) =>
    m GameLevel
removeItem = do
    env <- ask
    let tileSize = view eTileSize env
    
    currentLv <- use gCurrentLevel
    playerPos <- use (gPlayerState . pPosition)
    
    return $ filter
        (\ (cell, cellType) -> not ((cellType == '%' || cellType == 'k') &&
            isHit cell playerPos tileSize)) -- TODO: Rework this logic
        currentLv
    

updateSpeedY :: (MonadRWS Environment [String] GameState m) =>
    m Float
updateSpeedY = do
    env <- ask
    let tileSize = view eTileSize env
    
    delta <- use gDeltaSec
    
    (posX, posY) <- use (gPlayerState . pPosition )
    (spdX, spdY) <- use (gPlayerState . pSpeed    )
    (dirX, dirY) <- use (gPlayerState . pDirection)
    
    collideWithGround <- isCollision (posX, posY + spdY) '*'
    collideWithGrass  <- isCollision (posX, posY + spdY) 'a'
    return $ if collideWithGround || collideWithGrass
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
        if (cellType == '*' || cellType == 'a') &&
            isHit pnt cell tileSize
            then Just cell
            else next) Nothing $ level
        
    

isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize =
    x1            < x2 + tileSize &&
    x1 + tileSize > x2            &&
    y1            < y2 + tileSize &&
    y1 + tileSize > y2

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
    

openDoor :: GameState -> GameLevel
openDoor = undefined

updateKeys :: GameState -> GameState
updateKeys = undefined

