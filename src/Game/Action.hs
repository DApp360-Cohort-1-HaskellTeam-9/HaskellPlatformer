{-# LANGUAGE FlexibleContexts #-}

module Game.Action where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Logic

import Graphics.Gloss

move :: (MonadRWS Environment [String] GameState m) =>
    m Point
move = undefined -- do
    -- delta <- use gDeltaSec
    -- (posX, posY) <- use (gPlayerState . tmpPos)
    -- (velX, velY) <- use (gPlayerState . tmpVel)
    -- (dirX, dirY) <- use (gPlayerState . tmpDir)
    -- let posX' = posX + velX * dirX * delta
    -- let posY' = posY + velY * dirY * delta
    -- return (posX', posY')

jump :: undefined
jump = undefined

moveX :: (MonadRWS Environment [String] GameState m) =>
    m Point
moveX = do
    env <- ask
    let tileSize = view eTileSize env
    
    face <- use (gPlayerState . pHeading)
    
    (posX, posY) <- use (gPlayerState . pPosition)
    (spdX, spdY) <- use (gPlayerState . pSpeed)
    
    let next = case face of
            FaceLeft -> -spdX + posX
            FaceRight -> spdX + posX
    let hit = case face of
            FaceLeft  ->  tileSize
            FaceRight -> -tileSize
    
    collision <- playerCollision (next, posY)
    return $ case collision of
        Nothing -> (next, posY)
        Just tl -> (fst tl + hit, posY)
    

moveY :: (MonadRWS Environment [String] GameState m) =>
    Point -> m Point
moveY pnt = do
    env <- ask
    let tileSize = view eTileSize env
    
    (spdX, spdY) <- use (gPlayerState . pSpeed)
    
    collision <- playerCollision (fst pnt, snd pnt + spdY)
    return $ case collision of
        Nothing -> (fst pnt, snd pnt + spdY)
        Just tl -> if spdY < 0 -- is falling (moving down)
            then (fst pnt, snd tl + tileSize)
            else pnt
        
    
