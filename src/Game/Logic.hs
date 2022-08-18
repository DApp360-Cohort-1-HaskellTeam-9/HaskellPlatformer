module Game.Logic where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Asset
import Game.Data.Environment
import Game.Data.State
import Game.Init
import Game.Util
import Game.Data.Enum

import Graphics.Gloss

removeItem :: (PureRWS m) => m GameLevel
removeItem = do
    env <- ask
    let tileSize  = view eTileSize env
    let itemTiles = getCoinCellType ++
                    getKeyCellType  ++
                    getLifeUpCellType
    
    currentLv <- use (gLevelState . lLevelCells)
    playerPos <- use (gPlayerState . pPosition)
    
    return $ filter
        (\ (cell, cellType) -> not ((cellType `elem` itemTiles) 
        && isHit cell playerPos tileSize))
        currentLv
    

collideWith :: (PureRWS m) => [CellType] -> Point -> m (Maybe Cell)
collideWith colliders point = do
    env <- ask
    let tileSize = view eTileSize env
    
    level <- use (gLevelState . lLevelCells)
    return $ foldr (\ (cell, cellType) next ->
        if cellType `elem` colliders && isHit point cell tileSize
            then Just (cell, cellType)
            else next)
        Nothing
        level
    

openDoor :: RWSIO Bool
openDoor = do
    gs <- get
    collectedKeys <- use (gPlayerState . pCollectedKeys)
    totalKeys     <- use gTotalKeys
    currentLevel  <- use (gLevelState . lLevelCells)
    
    if collectedKeys == totalKeys
        then do
            isDoorOpen <- use gDoorOpen
            unless isDoorOpen $ do
                when (totalKeys > 0) $
                    logDebug "Opening the door"
                -- ALUT
                playSound DoorOpen
                -- ENDALUT
            return True
        else do
            return False
        
    

checkDoor :: (PureRWS m) => m ()
checkDoor = do
    let door = getDoorCellType
    player  <- use (gPlayerState . pPosition)
    hitDoor <- collideWith door player
    case hitDoor of
        Just dr -> do
            isDoorOpen <- use gDoorOpen
            when isDoorOpen $ do
                env <- ask
                
                currLives    <- use (gPlayerState . pLives)
                gPlayerState .= initPlayer
                gPlayerState .  pLives  .= currLives -- restore life count
                
                gParallax    .= (0, 0)
                gTransition  .= 1
                
                currLevel    <- use (gLevelState . lLevelName)
                case currLevel of
                    Level3 -> do
                        gSec       .= 0 -- reset sec
                        gGameScene .= SceneCredits
                    _      -> do
                        let nextLevel = flip runReader env . loadLevel . succ $ currLevel
                        gLevelState  .= nextLevel
                        
                        let lvCells   = view lLevelCells nextLevel
                            keyType   = getKeyCellType
                        gTotalKeys   .= levelItemCount lvCells keyType
                        gDoorOpen    .= False
        _ -> return ()
    

-- incCoin :: RWSIO Int
-- incCoin = do
--     env       <- ask
--     playerPos <- use (gPlayerState . pPosition)
--     -- collectedKeys <- use (gPlayerState . pCollectedKeys)
    
--     let coinCell = getCoinCellType
--     coinFound <- collideWith coinCell playerPos
    
--     case coinFound of
--         Nothing -> do
--             return 0
--         Just _  -> do
--             playSound Coin
--             return 1
        

incKeys :: (PureRWS m) => m ()
incKeys = do
    env           <- ask
    playerPos     <- use (gPlayerState . pPosition     )
    collectedKeys <- use (gPlayerState . pCollectedKeys)
    
    let keyCell = getKeyCellType -- view (eSprites . aKey) env
    keyFound <- collideWith keyCell playerPos
    
    case keyFound of
        Just _  -> do
            let keys   = collectedKeys + 1
            totalKeys <- use gTotalKeys
            logDebug $ "Collected keys " ++ show keys ++ " / " ++ show totalKeys
            gPlayerState . pCollectedKeys .= keys
        Nothing -> do
            return ()
        
    

incLives :: (PureRWS m) => m ()
incLives = do
    env <- ask
    playerPos   <- use (gPlayerState . pPosition)
    playerLives <- use (gPlayerState . pLives)
    getLife     <- collideWith getLifeUpCellType playerPos
    case getLife of
        Just _  -> gPlayerState . pLives %= (+1)
        Nothing -> return ()
    

timeUp :: (PureRWS m) => m ()
timeUp = do
    env   <- ask
    scene <- use gGameScene
    timeRemaining <- use gTimeRemaining

    let gameLose = (\x -> if x <= 0 then True else False) timeRemaining

    case gameLose of
        True -> do
            gGameScene .= SceneLose
            --logDebug $ "game lost"
        False -> do
            return ()
            --logDebug $ "continue"
    return ()
