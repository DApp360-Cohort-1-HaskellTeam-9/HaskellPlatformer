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
    imgs <- loadImgs -- Temporary
        {--- TODO: Asset management
            (_aBaseTiles . _eSprites $ env) ++
            [_aKey . _eSprites $ env] ++
            (_aPlayer . _eSprites $ env)-}
    let level = _gCurrentLevel gs
    let playerState = _gPlayerState gs -- Get playerState from gameState
    let playerPos   = _pPosition playerState
    --let heading = _pHeading playerState
    --let spriteIndex = _pSpriteIndex playerState
    return $ 
        (pictures $ [translate 0 0 (imgs !! 6)] ++ 
        (map (\ cell -> renderTile cell imgs) (level) ++ 
        [translate (fst (playerPos)) (snd (playerPos)) (color yellow $ rectangleSolid 32 32)]))
        --[translate (fst (playerPos)) (snd (playerPos)) (imgs !! (fromIntegral spriteIndex + 6 ))]  --(imgs !! (fromIntegral spriteIndex + 6 + isRight (heading))))

    {-return $ pictures (
        
        (uncurry translate playerPos . color red $ rectangleSolid 32 32) :
         map (\ cell -> renderTile cell imgs
            drawTile cell 
            (head imgs) -- TODO: Get rid of partial functions
            (color yellow $ rectangleSolid 32 32))
            level
        )
        -}

{-
renderGame :: GameState -> [Picture] -> IO Picture
renderGame gs imgs = do
    return $ 
     (pictures $ [translate 0 0 (imgs !! 6)] ++ (map (\ cell -> renderTile cell imgs) (currentLevel gs) --Tiles
     ++ 
     [translate (fst (position gs)) (snd (position gs)) (imgs !! (spriteCount gs + 6 + isRight (heading gs)))]
     )
     ) 
-}

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
renderTile :: Cell -> [Picture] -> Picture
renderTile (pos, cellType) imgs =
    let baseImg = imgs !! 0
        grassImg = imgs !! 1
        coinImg = imgs !! 2
        keyImg = imgs !! 3
        doorCTImg = imgs !! 4
        doorCMImg = imgs !! 5
    in
    uncurry translate (pos) $
    case cellType of
     '*' -> baseImg 
     'a' -> grassImg
     '%' -> coinImg 
     'k' -> keyImg
     't' -> doorCTImg
     'b' -> doorCMImg

{-
--Enemies to appear at random times
renderEnemy :: undefined
renderEnemy = undefined
-}

{-
drawTile :: Cell -> Picture -> Picture -> Picture
drawTile cell tileImg keyImg =
    uncurry translate (fst cell) (checkImg cell tileImg keyImg)

checkImg :: Cell -> Picture -> Picture -> Picture
checkImg (_, cellType) tile key =
    if cellType == '*'
        then tile
        else key
-}  

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
