{-# LANGUAGE FlexibleContexts #-}

module Game.Logic where

import Control.Lens
import Control.Monad.RWS
-- import Control.Monad.Extra

import Game.AssetManagement
import Game.Data.Environment
import Game.Data.State

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
    

collideWith :: (MonadRWS Environment [String] GameState m) =>
    [CellType] -> Point -> m (Maybe Point)
collideWith colliders (x, y) = do
    env <- ask
    let tileSize = view eTileSize env

    level <- use gCurrentLevel
    return $ foldr (\ (cell@(row, col), cellType) next ->
        if cellType `elem` colliders &&
            x            < row + tileSize &&
            x + tileSize > row            &&
            y            < col + tileSize &&
            y + tileSize > col
            then Just cell
            else next)
        Nothing
        level
    

-- Helper func. for removeItem
-- TODO: Use collideWith later
isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize =
    x1            < x2 + tileSize &&
    x1 + tileSize > x2            &&
    y1            < y2 + tileSize &&
    y1 + tileSize > y2

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
    let keyCell = getKeyCellType -- view (eSprites . aKey) env
    keyFound <- collideWith keyCell playerPos

    return $ case keyFound of
        Nothing -> collectedKeys
        Just _  -> collectedKeys + 1
    
