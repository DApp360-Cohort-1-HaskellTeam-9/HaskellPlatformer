module Game.Action where

import Control.Lens
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Logic

movePlayer :: (PureRWS m) => m ()
movePlayer = do
    env <- ask
    let tileSize = view eTileSize env
    
    delta <- use gDeltaSec
    
    (posX, posY) <- use (gPlayerState . pPosition) -- initial position
    (spdX, spdY) <- use (gPlayerState . pSpeed   ) -- initial speed
    (incX, incY) <- use (gPlayerState . pIncSpeed) -- parameters
    (maxX, maxY) <- use (gPlayerState . pMaxSpeed) -- parameters
    
    -- current player X-axis state
    movement     <- use (gPlayerState . pMovement)
    face         <- use (gPlayerState . pHeading )
    
    g <- use gForce
    let spdX' = case movement of
            MoveStop -> max    0 $ spdX - incX * delta
            _        -> min maxX $ spdX + incX * delta
    let spdY' =         max maxY $ spdY - incY * delta * g
    
    -- calculate next position
    let posX' = case face of
            FaceLeft -> -spdX' * delta + posX
            FaceRight -> spdX' * delta + posX
    let posY' =          spdY' * delta + posY
    
    -- check for collisions
    let colliders = getCollidables
    -- must be calculated independently for each axis
    hitX <- collideWith colliders (posX', posY)
    hitY <- collideWith colliders (posX, posY')
    
    -- recalculate X-Axis position and speed
    let dirX = case face of
            FaceLeft -> -1
            FaceRight -> 1
    let dirY = signum spdY'
    let (posX'', spdX'') = case hitX of -- reset speed on collision
            Just ((x, y), _) -> (x - tileSize * dirX, 0)
            Nothing          -> (posX', spdX')
    
    -- recalculate Y-Axis position and speed
    let (bounciness, bounceStop) = case hitY of
            Just (_, cellType) -> getBounciness cellType
            Nothing            -> (0, 0)
    let bounce  = bounciness * negate spdY'
    let bounce' = if abs bounce < bounceStop
                  then 0 else bounce
    let (posY'', spdY'') = case hitY of -- bounce on collision
            Just ((x, y), _) -> (y - tileSize * dirY, bounce')
            Nothing          -> (posY', spdY')
    
    -- update player position and speed
    gPlayerState . pPosition .= (posX'', posY'')
    gPlayerState . pSpeed    .= (spdX'', spdY'')

moveEnemies :: (PureRWS m) => m ()
moveEnemies = do
    currStates <- use gEnemies
    nextStates <- forM currStates moveEnemy
    gEnemies   .= nextStates

moveEnemy :: (PureRWS m) => EnemyState -> m EnemyState
moveEnemy enemy = do
    env <- ask
    let tileSize = view eTileSize env
    
    delta <- use gDeltaSec
    
    let (posX, posY) = view ePosition enemy -- initial position
        (spdX, spdY) = view eSpeed    enemy -- initial speed
        (incX, incY) = view eIncSpeed enemy -- parameters
        (maxX, maxY) = view eMaxSpeed enemy -- parameters
        face = view eHeading enemy -- heading
    
    g <- use gForce
    let spdX' = min maxX $ spdX + incX * delta
    let spdY' = max maxY $ spdY - incY * delta * g
    
    -- calculate next position
    let posX' = case face of
            FaceLeft -> -spdX' * delta + posX
            FaceRight -> spdX' * delta + posX
    let posY' =          spdY' * delta + posY

    -- check for collisions
    let colliders = getCollidables ++ getSpikesCellType
    -- must be calculated independently for each axis
    hitX <- collideWith colliders (posX', posY)
    hitY <- collideWith colliders (posX, posY')

    -- recalculate X-Axis position and speed
    let dirX = case face of
            FaceLeft -> -1
            FaceRight -> 1
    let dirY = signum spdY'
    let (posX'', spdX'') = case hitX of -- reset speed on collision
            Just ((x, y), _) -> (x - tileSize * dirX, 0)
            Nothing          -> (posX', spdX')
        face' = case hitX of -- reverse heading on wall collision
            Just _  -> case face of
                FaceRight -> FaceLeft
                FaceLeft -> FaceRight
            Nothing -> face

    
    -- recalculate Y-Axis position and speed
    let (bounciness, bounceStop) = case hitY of
            Just (_, cellType) -> getBounciness cellType
            Nothing            -> (0, 0)
    let bounce  = bounciness * negate spdY'
    let bounce' = if abs bounce < bounceStop
                  then 0 else bounce
    let (posY'', spdY'') = case hitY of -- bounce on collision
            Just ((x, y), _) -> (y - tileSize * dirY, bounce')
            Nothing          -> (posY', spdY')
        
    
    -- return updated enemy position and speed
    return enemy
        { _ePosition = (posX'', posY'')
        , _eSpeed    = (spdX'', spdY'')
        , _eHeading  = face'
        }
    
