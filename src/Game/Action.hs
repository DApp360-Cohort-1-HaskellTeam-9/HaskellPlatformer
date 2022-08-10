{-# LANGUAGE FlexibleContexts #-}

module Game.Action where

import Control.Lens
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Logic

movePlayer :: (MonadRWS Environment [String] GameState m) =>
    m ()
movePlayer = do
    env <- ask
    let tileSize = view eTileSize env
    
    (posX, posY) <- use (gPlayerState . pPosition)
    (pos', spd') <- calcNextPlayerPosSpd
    
    let (posX', posY') = pos' -- next position candidate
    let (spdX', spdY') = spd' -- next speed candidate
    
    -- check for collisions
    let colliders = getCollidables
    -- must be calculated independently for each axis
    hitX <- collideWith colliders (posX', posY)
    hitY <- collideWith colliders (posX, posY')
    
    -- recalculate position and speed
    face <- use (gPlayerState . pHeading)
    let dirX = case face of
            FaceLeft -> -1
            FaceRight -> 1
    let dirY = signum spdY'
    let (posX'', spdX'') = case hitX of -- reset speed on collision
            Just (x, _) -> (x - tileSize * dirX, 0)
            Nothing     -> (posX', spdX')
    bounciness <- use (gPlayerState . pBounciness  )
    bounceStop <- use (gPlayerState . pBounceCutoff)
    let bounce  = bounciness * negate spdY'
    let bounce' = if bounce < bounceStop && bounce > -bounceStop
                  then 0 else bounce
    let (posY'', spdY'') = case hitY of -- bounce on collision
            Just (_, y) -> (y - tileSize * dirY, bounce')
            Nothing     -> (posY', spdY')
    
    -- update player position and speed
    gPlayerState . pPosition .= (posX'', posY'')
    gPlayerState . pSpeed    .= (spdX'', spdY'')

calcNextPlayerPosSpd :: (MonadRWS Environment [String] GameState m) =>
    m (XY, XY)
calcNextPlayerPosSpd = do
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
    
    let nextPos = (posX', posY')
    let nextSpd = (spdX', spdY')
    return (nextPos, nextSpd)

jump :: undefined
jump = undefined
