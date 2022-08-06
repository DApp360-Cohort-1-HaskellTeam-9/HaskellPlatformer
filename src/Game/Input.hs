module Game.Input where

import Control.Monad.RWS

import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss.Interface.IO.Game

handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pDirection = (-1, 0)
            , _pHeading = FaceLeft
            }
    return $ gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pDirection = (1, 0)
            , _pHeading = FaceRight
            }
    return $ gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        nextPlayerState = currPlayerState
            { _pSpeed = (fst . _pSpeed $ currPlayerState, 5)
            }
    return $ gs { _gPlayerState = nextPlayerState }
handleKeys (EventKey (SpecialKey KeyLeft)  Up _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        stopPlayerMove = currPlayerState
            { _pDirection = (0, 0)
            }
    return $ case _pDirection currPlayerState of
        (-1, 0) -> gs { _gPlayerState = stopPlayerMove }
        _       -> gs
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) = do
    gs <- get
    let currPlayerState = _gPlayerState gs
        stopPlayerMove = currPlayerState
            { _pDirection = (0, 0)
            }
    return $ case _pDirection currPlayerState of
        (1, 0) -> gs { _gPlayerState = stopPlayerMove }
        _      -> gs
handleKeys _ = do
    gs <- get
    return gs
-- exitGame??
-- pauseGame??
