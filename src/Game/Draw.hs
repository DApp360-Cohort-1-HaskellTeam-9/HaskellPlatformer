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
import Game.Data.Enum

import Graphics.Gloss

renderGame :: RWSIO Picture
renderGame = do
    env          <- ask
    level        <- use (gLevelState . lLevelCells)
    tiles        <- mapM drawTile level
    playerPos    <- use (gPlayerState . pPosition)
    playerSprite <- getPlayerSprite
    text         <- renderText
    background   <- renderBackground
    timer        <- renderTimer
    scene        <- use (gGameScene)

    let game = pictures $ 
            case scene of 
                    ScenePause -> 
                        background ++ 
                        uncurry translate playerPos playerSprite : 
                        tiles ++ 
                        text
                    SceneStart      ->
                        background ++ 
                        text
                    SceneCredits    ->
                        background ++ 
                        text
                    SceneLevel      ->
                        background ++ 
                        uncurry translate playerPos playerSprite :
                        tiles ++ 
                        text ++
                        timer        
                    SceneWin        ->
                        tiles ++ 
                        background ++ 
                        text
                    SceneLose       ->
                        tiles ++ 
                        background ++ 
                        text
                    SceneTransition ->
                        [] --Not sure
    return game

updateGame :: Float -> RWSIO GameState
updateGame sec = do
    gs <- get
    
    gDeltaSec .= sec -- might need this for other screen states
                     -- normally, the value should be 1/FPS
    timeRemaining <- use gTimeRemaining
    gTimeRemaining .= timeRemaining - sec
    scene <- use gGameScene
    
    case scene of
        ScenePause -> 
            return gs
        _       -> do
            movePlayer
            incPlayerSprite
            --playSFX
            
            keys <- incKeys
            gPlayerState . pCollectedKeys .= keys
            
            updatedLevel  <- removeItem
            gLevelState . lLevelCells .= updatedLevel
            
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
        doorImgs = view (eSprites . aDoor) env

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

renderText :: (MonadRWS Environment [String] GameState m) =>
    m [Picture]
renderText = do
    env          <- ask
    scene       <- use gGameScene
    level        <- use (gLevelState . lLevelName)
    let continue  = view (eSprites . aTxtPause) env
    let title     = view (eSprites . aTxtTitle) env
    let enter     = view (eSprites . aTxtEnter) env
    let startText = [uncurry translate (0,200) title, uncurry translate (0,-200) enter] 

    case scene of
        ScenePause  -> case level of
                    LevelStart  -> return startText --Add credits screen?
                    _           -> return [continue]
        _           -> case level of
                    LevelStart  -> return startText 
                    _           -> return []

renderBackground :: (PureRWS m) => m [Picture]
renderBackground = do
    env      <- ask
    level    <- use (gLevelState . lLevelName)
    scene    <- use (gGameScene)   
    
    let lvlList = view (eSprites . aLvlNames) env
    let bgImgs = view (eSprites . aBgImg) env
    let zipLvls = zip lvlList bgImgs
    let imgToUse = lookup (show level) zipLvls

    case imgToUse of
        Nothing -> return []
        Just x  -> return [x]

renderDigits :: String -> [Picture] -> [Picture]
renderDigits [] _ = []
renderDigits (x:xs) digits 
            | x == '-'  = [digits !! 0]                               -- keep showing 0 when timer goes negative
            | otherwise = digits !! read [x] : renderDigits xs digits

addShift :: [Picture] -> Float -> Float -> [Picture]
addShift [] _ _ = []  -- 30 is width of digit picture
addShift (x:xs) xPos yPos = (uncurry translate (xPos - fromIntegral (30 * length xs), yPos) x) : (addShift xs xPos yPos)

renderTimer :: (PureRWS m) => m [Picture]
renderTimer = do
    env <- ask
    timeRemaining <- use gTimeRemaining
    let timerText = show . round $ timeRemaining
    let windowWidth = view eWindowWidth env
    let windowHeight = view eWindowHeight env
    let tileSize     = view eTileSize env
    let digits = view (eSprites . aTxtDigits) env
    let timerPics = renderDigits timerText digits
    let xPos = fromIntegral windowWidth / 2  - tileSize / 2
    let yPos = fromIntegral windowHeight / 2 - tileSize / 2
    let timer = addShift timerPics xPos yPos
    return timer

{-
playSFX :: RWSIO ()
playSFX = do
    player <- use (gPlayerState . pPosition)
    let coin = getCoinCellType
        key  = getKeyCellType
        door = getDoorCellType
    
    hitCoin <- collideWith coin player
    case hitCoin of
        Just cn -> playSound Coin
        Nothing -> return ()
    
    hitKey <- collideWith key player
    case hitKey of
        Just ky -> playSound Key
        Nothing -> return ()
    
    hitDoor <- collideWith door player
    isDoorOpen <- use gDoorOpen
    when isDoorOpen $ case hitDoor of
        Just cn -> playSound DoorClose
        Nothing -> return ()
-}
