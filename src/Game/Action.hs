module Game.Action where

import Control.Monad.RWS

import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Logic

import Graphics.Gloss

move :: RWS Environment [String] GameState Point
move = undefined

jump :: undefined
jump = undefined

moveX :: PlayerFacing -> GameState -> Point
moveX face gs = -- TODO: MonadRWS
    let
        playerState  = _gPlayerState gs
        (spdX, spdY) = _pSpeed playerState
        (posX, posY) = _pPosition playerState
        next = case face of
            FaceLeft -> -spdX + posX
            FaceRight -> spdX + posX
        hit = case face of -- TODO: _eTileSize
            FaceLeft  ->  32
            FaceRight -> -32
    in
        case playerCollision gs (next, posY) '*' of
            Nothing -> case playerCollision gs (next, posY) 'a' of
                        Nothing -> (next, posY) 
                        Just tl -> (fst tl + hit, posY)
            Just tl -> (fst tl + hit, posY)

{-
        case playerCollision gs (next, posY) '*' of
            Nothing -> (next, posY)
            Just tl -> (fst tl + hit, posY)
-}      
    

moveY :: GameState -> Point -> Point
moveY gs pnt =
    let
        playerState  = _gPlayerState gs
        (spdX, spdY) = _pSpeed playerState
    in
        case playerCollision gs (fst pnt, snd pnt + spdY) '*' of
            Nothing -> case playerCollision gs (fst pnt, snd pnt + spdY) 'a' of
                        Nothing -> (fst pnt, snd pnt + spdY)
                        Just tl -> if spdY < 0 -- is falling (moving down)
                            then (fst pnt, snd tl + 32) -- _eTileSize
                            else pnt
            Just tl -> if spdY < 0 -- is falling (moving down)
                            then (fst pnt, snd tl + 32) -- _eTileSize
                            else pnt


{-
        case playerCollision gs (fst pnt, snd pnt + spdY) '*' of
            Nothing -> (fst pnt, snd pnt + spdY)
            Just tl -> if spdY < 0 -- is falling (moving down)
                then (fst pnt, snd tl + 32) -- _eTileSize
                else pnt
-}
            
        
    
