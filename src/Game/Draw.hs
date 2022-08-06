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
    let imgs = -- TODO: Asset management
            [ _aBase . _eSprites $ env
            , _aKey . _eSprites $ env
            ] ++ -- concat to player sprites
            (_aPlayer . _eSprites $ env)
    let level = _gCurrentLevel gs
    let playerState = _gPlayerState gs
    let playerPos   = _pPosition playerState
    return $ pictures (
        (uncurry translate playerPos . color red $ rectangleSolid 32 32) :
         map (\ cell ->
            drawTile cell
            (head imgs) -- TODO: Get rid of partial functions
            (color yellow $ rectangleSolid 32 32))
            level
        )
    

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
drawTile :: Cell -> Picture -> Picture -> Picture
drawTile cell tileImg foodImg =
    uncurry translate (fst cell) (checkImg cell tileImg foodImg)

renderTile :: undefined
renderTile = undefined

checkImg :: Cell -> Picture -> Picture -> Picture
checkImg (_, cellType) tile key =
    if cellType == '*'
        then tile
        else key
    

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
