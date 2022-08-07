module Game.Logic where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

removeItem :: GameState -> GameLevel -- TODO: MonadRWS
removeItem gs =
    let
        playerState = _gPlayerState gs
        pos = _pPosition playerState
    in
        filter
            (\ cell -> not $ isHit (fst cell) pos && snd cell == '%')
            (_gCurrentLevel gs)
        
    

updateSpeedY :: GameState -> Float -- TODO: MonadRWS
updateSpeedY gs =
    let
        playerState  = _gPlayerState gs
        (posX, posY) = _pPosition playerState
        (spdX, spdY) = _pSpeed playerState
    in
        if isCollision gs (posX, posY + spdY) '*'
            then negate . abs $ spdY -- only negate upwards movement
                else if isCollision gs (posX, posY + spdY) 'a'
                    then negate . abs $ spdY -- only negate upwards movement 
            else max (-5) $ spdY - 0.05 -- TODO: Don't use constants!
        
    

updateSpeedX :: GameState -> Float -- TODO: MonadRWS
updateSpeedX gs =
    let
        playerState = _gPlayerState gs
        dir = _pDirection playerState
        (spdX, spdY) = _pSpeed playerState
    in
        case dir of
            (0, 0) -> max 0 $ spdX - 0.05
            _ -> min 2.5 $ spdX + 0.05
        
    

-- TODO: MonadRWS
playerCollision :: GameState -> Point -> CellType -> Maybe Point
playerCollision gs pnt checkType = for . _gCurrentLevel $ gs where
    for [] = Nothing
    for ((tile, tileType):nextTile) = if tileType == checkType && isHit pnt tile
        then Just tile
        else for nextTile
    

-- TODO: _eTileSize
isHit :: Point -> Point -> Bool
isHit (b1x, b1y) (b2x, b2y) =
    b1x      < b2x + 32 &&
    b1x + 32 > b2x      &&
    b1y      < b2y + 32 &&
    b1y + 32 > b2y

-- TODO: Do we really need 3 different functions to check for collision?
isCollision :: GameState -> Point -> CellType -> Bool
isCollision gs pnt checkType = any
    (\((x, y), tileType) -> tileType == checkType && isHit pnt (x, y))
    (_gCurrentLevel gs)


openDoor :: GameState -> GameLevel
openDoor = undefined

updateKeys :: GameState -> GameState
updateKeys = undefined

