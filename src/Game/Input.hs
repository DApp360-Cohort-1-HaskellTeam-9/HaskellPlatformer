module Game.Input where

import Control.Monad.RWS
import Control.Lens

import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss.Interface.IO.Game


--TODO: Add text "Press 'p' to continue" or similar. 
handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys e = do
    currPlayerState <- use gPlayerState
    isPaused <- use gPaused
    gs <- get
    case isPaused of
        True -> case e of
                (EventKey (Char 'p') Down _ _)  -> 
                    gPaused .= False  
                _                               -> 
                    return ()

        False -> case e of
                (EventKey (Char 'p') Down _ _)              -> do
                    gPaused .= True

                (EventKey (SpecialKey KeyLeft) Down _ _)    -> do
                    gPlayerState . pDirection .= (-1, 0)
                    gPlayerState . pHeading .= FaceLeft 

                (EventKey (SpecialKey KeyRight) Down _ _)   -> do
                    gPlayerState . pDirection .= (1, 0)
                    gPlayerState . pHeading .= FaceRight 

                (EventKey (SpecialKey KeyUp) Down _ _)      -> do
                    gPlayerState . pSpeed .= 
                        (fst . _pSpeed $ currPlayerState, 15)

                (EventKey (SpecialKey KeyLeft) Up _ _)      -> do
                    gPlayerState . pDirection .= (0, 0)

                (EventKey (SpecialKey KeyRight) Up _ _)     -> do
                    gPlayerState . pDirection .= (0, 0)
                    
                _                                           -> do
                    return ()
    newState <- get
    return newState

{-
handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys (EventKey (Char 'p') Down _ _) = do
    gs <- get
    let isPaused = _gPaused gs
    let pauseState =
            case isPaused of
                True    -> gs { _gPaused = False}
                False   -> gs { _gPaused = True}
    return pauseState
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
            { _pSpeed = (fst . _pSpeed $ currPlayerState, 15)
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
-}


-- exitGame??


