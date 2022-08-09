module Game.Input where

import Control.Lens
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Logic

import Graphics.Gloss.Interface.IO.Game

handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pMovement = MoveLeft
            , _pHeading = FaceLeft
            }
    return $ gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pMovement = MoveRight
            , _pHeading = FaceRight
            }
    return $ gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pSpeed = (fst . _pSpeed $ currPlayerState, 2000)
            }
    -- temporary solution for ground checking
    (x, y) <- use (gPlayerState . pPosition)
    let colliders = getCollidables
    hit <- collideWith colliders (x, y - 1)
    return $ case hit of
        Nothing -> gs
        _ -> gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyLeft)  Up _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        stopPlayerMove = currPlayerState
            { _pMovement = MoveStop
            }
    return $ case _pMovement currPlayerState of
        MoveLeft -> gs { _gPlayerState = stopPlayerMove }
        _        -> gs
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        stopPlayerMove = currPlayerState
            { _pMovement = MoveStop
            }
    return $ case _pMovement currPlayerState of
        MoveRight -> gs { _gPlayerState = stopPlayerMove }
        _         -> gs
handleKeys _ = do
    gs <- get
    return gs
-- exitGame??
-- pauseGame??
