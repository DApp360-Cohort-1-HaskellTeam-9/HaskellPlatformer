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
    env          <- ask
    level        <- use gCurrentLevel
    tiles        <- mapM drawTile level
    playerPos    <- use (gPlayerState . pPosition)
    playerSprite <- getPlayerSprite
    continue     <- renderContinue
    return . pictures $ 
     head (view (eSprites . aBgImg) env) : 
     uncurry translate playerPos playerSprite : 
     tiles ++ [continue]

updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame sec = do
    gs <- get

    paused <- use gPaused

    case paused of
        True -> 
            return gs
        False -> do
            gDeltaSec .= sec
                
            posX <- moveX
            next <- moveY posX
            gPlayerState  . pPosition .= next
                
            spdX <- updateSpeedX
            spdY <- updateSpeedY
            gPlayerState  . pSpeed .= (spdX, spdY)

            keys <- incKeys
            gPlayerState . pCollectedKeys .= keys

            updatedLevel  <- removeItem
            gCurrentLevel .= updatedLevel

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
        doorImgs = (view (eSprites . aDoor) env)

    isDoorOpen <- use gDoorOpen

    doorTup <- getDoorSprite

    return $ case cellType of
        '*' -> baseImg 
        '^' -> grassImg
        'c' -> coinImg
        'k' -> fst keyImg
        't' -> fst doorTup
        'b' -> snd doorTup
        _   -> circle 0

drawTile :: (MonadRWS Environment [String] GameState m) =>
    Cell -> m Picture
drawTile (pos, celltYpe) = do
    tile <- renderTile celltYpe
    return . uncurry translate pos $ tile

--TEXT : uncurry translate pos $ scale 0.2 0.2 $ text "TESTING 123!"

renderContinue :: (MonadRWS Environment [String] GameState m) =>
    m Picture
renderContinue = do
    env      <- ask
    paused   <- use gPaused
    let continue = view (eSprites . aTxtCont) env
    case paused of
        True  -> return $ continue
        False -> return $ pictures []