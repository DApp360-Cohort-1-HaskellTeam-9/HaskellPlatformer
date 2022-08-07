{-# LANGUAGE FlexibleContexts #-}

module Game.Draw where

import Control.Lens
import Control.Monad.RWS

import Game.Action
import Game.AssetManagement
import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State
import Game.Logic

import Graphics.Gloss

renderGame :: RWST Environment [String] GameState IO Picture
renderGame = do
    env <- ask
    
    level <- use gCurrentLevel
    tiles <- mapM drawTile level
    
    playerPos <- use (gPlayerState . pPosition)
    
    return . pictures $
        view (eSprites . aBgImg) env :
        uncurry translate playerPos (color red $ rectangleSolid 32 32) :
        tiles
    

updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame sec = do
    gs <- get
    
    gDeltaSec .= sec
    
    posX <- moveX
    next <- moveY posX
    gPlayerState  . pPosition .= next
    
    spdX <- updateSpeedX
    spdY <- updateSpeedY
    gPlayerState  . pSpeed .= (spdX, spdY)
    
    updatedLevel  <- removeItem
    gCurrentLevel .= updatedLevel
    
    nextState <- get
    return nextState

-- Helper Functions:
renderTile :: (MonadRWS Environment [String] GameState m) =>
    CellType -> m Picture
renderTile cellType = do
    env <- ask
    let baseImg  = view (eSprites . aBase ) env
        grassImg = view (eSprites . aGrass) env
        coinImg  = view (eSprites . aCoin ) env
        keyImg   = view (eSprites . aKey  ) env
    -- isDoorOpen <- use gDoorOpen
    -- let doorImg = if isDoorOpen
    --     then head $ view (eSprites . aDoor ) env
    --     else last $ view (eSprites . aDoor ) env
    let doorTop    = last $ view (eSprites . aDoor) env
    let doorBottom = head $ view (eSprites . aDoor) env
    return $ case cellType of
        '*' -> baseImg 
        'a' -> grassImg
        '%' -> coinImg 
        'k' -> keyImg
        't' -> doorTop
        'b' -> doorBottom
        _   -> circle 0
    

{-
--Enemies to appear at random times
renderEnemy :: undefined
renderEnemy = undefined
-}

drawTile :: (MonadRWS Environment [String] GameState m) =>
    Cell -> m Picture
drawTile (pos, celltYpe) = do
    tile <- renderTile celltYpe
    return . uncurry translate pos $ tile
