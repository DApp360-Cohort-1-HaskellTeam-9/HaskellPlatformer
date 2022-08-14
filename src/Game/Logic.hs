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
    let tileSize = view eTileSize env
    let itemTiles = getCoinCellType ++ getKeyCellType
    
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
    collectedKeys   <- use (gPlayerState . pCollectedKeys)
    totalKeys       <- use gTotalKeys
    currentLevel    <- use (gLevelState . lLevelCells)
    
    if collectedKeys == totalKeys
        then do
            isDoorOpen <- use gDoorOpen
            -- ALUT
            -- unless isDoorOpen $ playSound DoorOpen
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
                
                gPlayerState .= initPlayer
                gParallax     .= (0, 0)
                gTransition  .= 1
                
                currLevel    <- use (gLevelState . lLevelName)
                let nextLevel = flip runReader env . loadLevel . succ $ currLevel
                gLevelState  .= nextLevel
                
                let lvCells   = view lLevelCells nextLevel
                    keyType   = getKeyCellType
                gTotalKeys   .= levelItemCount lvCells keyType
                
                gDoorOpen    .= False
        Nothing -> return ()
    

-- incCoin :: RWST Environment [String] GameState IO Int
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
        

incKeys :: (PureRWS m) => m Int
incKeys = do
    env           <- ask
    playerPos     <- use (gPlayerState . pPosition     )
    collectedKeys <- use (gPlayerState . pCollectedKeys)
    
    let keyCell = getKeyCellType -- view (eSprites . aKey) env
    keyFound <- collideWith keyCell playerPos


    case keyFound of
        Nothing -> do
            return collectedKeys
        Just _  -> do
            let keys = collectedKeys + 1
            totalKeys <- use gTotalKeys
            tell ["Collected keys " ++ show keys ++ " / " ++ show totalKeys]
            return keys
        
    
