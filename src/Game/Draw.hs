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
    gs <- get
    env <- ask
    
    let level = _gCurrentLevel gs
    tiles <- mapM drawTile level
    
    let playerState = _gPlayerState gs
        playerPos   = _pPosition playerState
    
    return . pictures $
        view (eSprites . aBgImg) env :
        uncurry translate playerPos (color red $ rectangleSolid 32 32) :
        tiles
    

updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame sec = do
    gs <- get
    let currPlayerState = _gPlayerState gs
    let nextPlayerState = currPlayerState
            { _pSpeed = (updateSpeedX gs, updateSpeedY gs)
            , _pPosition = moveY gs $ moveX (_pHeading currPlayerState) gs
            }
    return gs
        { _gPlayerState  = nextPlayerState
        , _gCurrentLevel = removeItem gs
        }
    

-- Helper Functions:
--REPLACE !! with LENS?
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
        _ -> circleSolid 0
    

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

testRenderPureHelper :: (MonadRWS Environment [String] GameState m) =>
    m Picture
testRenderPureHelper = do
    env <- ask
    gs <- get
    tell ["log something"]
    playerSprite <- getPlayerSprite
    return . pictures $ []

testRenderIOHelper :: (MonadIO m) =>
    m ()
testRenderIOHelper = do
    liftIO . putStrLn $ ""
    return ()

testUpdatePureHelper :: (MonadRWS Environment [String] GameState m) =>
    Float -> m GameState
testUpdatePureHelper sec = do
    env <- ask
    
    prevSec <- use gSec   -- get gSec
    gSec    .= sec -- set gSec to sec
    
    let deltaSec = sec - prevSec
    gDeltaSec   .= deltaSec -- set gDeltaSec to sec - prevSec
    
    gs <- get
    let fps = fromIntegral $ view eFPS env
    gPlayerState . pSpriteIndex %= (+ deltaSec * fps) -- experiment with this
    
    return gs
