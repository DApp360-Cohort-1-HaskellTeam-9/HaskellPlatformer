{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Game.Draw where

import Control.Lens
import Control.Monad.RWS

import Game.Action
import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State
import Game.Logic

import Graphics.Gloss

renderGame :: RWSIO Picture
renderGame = do
    env          <- ask
    level        <- use gCurrentLevel
    tiles        <- mapM drawTile level
    playerPos    <- use (gPlayerState . pPosition)
    playerSprite <- getPlayerSprite
    continue     <- renderContinue
    background   <- renderBackground

    return . pictures $ 
        background ++ 
        uncurry translate playerPos playerSprite : 
        tiles ++ 
        continue
    

updateGame :: Float -> RWSIO GameState
updateGame sec = do
    gs <- get
    
    gDeltaSec .= sec -- might need this for other screen states
                     -- normally, the value should be 1/FPS
    
    paused <- use gPaused
    
    case paused of
        True -> 
            return gs
        False -> do
            movePlayer
            incPlayerSprite
--            playSFX
            
            keys <- incKeys
            gPlayerState . pCollectedKeys .= keys
            
            updatedLevel  <- removeItem
            gCurrentLevel .= updatedLevel
            
            door <- openDoor
            gDoorOpen .= door
            
            nextState <- get
            return nextState
        
    

-- Helper Functions:
renderTile :: (PureRWS m) => CellType -> m Picture
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
        _   -> circle 0 -- should never reach here

drawTile :: (PureRWS m) => Cell -> m Picture
drawTile (pos, celltYpe) = do
    tile <- renderTile celltYpe
    return . uncurry translate pos $ tile

--TEXT : uncurry translate pos $ scale 0.2 0.2 $ text "TESTING 123!"

renderContinue :: (PureRWS m) => m [Picture]
renderContinue = do
    env      <- ask
    paused   <- use gPaused
    let continue = view (eSprites . aTxtCont) env
    case paused of
        True  -> return [continue]
        False -> return []

renderBackground :: (PureRWS m) => m [Picture]
renderBackground = do
    env      <- ask
    level   <- use gLevelName

    let lvlList = view (eSprites . aLevels) env
    let bgImgs = view (eSprites . aBgImg) env
    let zipLvls = zip lvlList bgImgs
    let imgToUse = lookup level zipLvls

    case imgToUse of
        Nothing -> return []
        Just x  -> return [x]
    

--playSFX :: RWSIO ()
--playSFX = do
--    player <- use (gPlayerState . pPosition)
--    let coin = getCoinCellType
--        key  = getKeyCellType
--        door = getDoorCellType
--    
--    hitCoin <- collideWith coin player
--    case hitCoin of
--        Just cn -> playSound Coin
--        Nothing -> return ()
--    
--    hitKey <- collideWith key player
--    case hitKey of
--        Just ky -> playSound Key
--        Nothing -> return ()
--    
--    hitDoor <- collideWith door player
--    isDoorOpen <- use gDoorOpen
--    when isDoorOpen $ case hitDoor of
--        Just cn -> playSound DoorClose
--        Nothing -> return ()
    
