{-# LANGUAGE FlexibleContexts #-}

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
    isPaused <- use gPaused
    case e of
        (EventKey (Char 'p') Down _ _) -> do
            pauseGame
        _                              -> 
            case isPaused of
                True -> return ()
                False -> case e of 
                        (EventKey (SpecialKey KeyLeft) Down _ _)    -> do
                            moveLeft
                        (EventKey (SpecialKey KeyRight) Down _ _)   -> do
                            moveRight
                        (EventKey (SpecialKey KeyUp) Down _ _)      -> do
                            moveUp
                        (EventKey (SpecialKey KeyLeft) Up _ _)      -> do
                            stopMoveLeft
                        (EventKey (SpecialKey KeyRight) Up _ _)     -> do
                            stopMoveRight
                        _                                           -> do
                            return ()
    newState <- get
    return newState


pauseGame :: (MonadRWS Environment [String] GameState m) => m ()
pauseGame = do
    isPaused <- use gPaused
    case isPaused of
        False -> gPaused .= True
        True -> gPaused .= False

moveUp :: (MonadRWS Environment [String] GameState m) => m ()
moveUp = do
    currPlayerState <- use gPlayerState
    gPlayerState . pSpeed .= (fst . _pSpeed $ currPlayerState, 15)

moveLeft :: (MonadRWS Environment [String] GameState m) => m ()
moveLeft = do
    gPlayerState . pDirection .= (-1, 0)
    gPlayerState . pHeading .= FaceLeft

moveRight :: (MonadRWS Environment [String] GameState m) => m () 
moveRight = do
    gPlayerState . pDirection .= (1, 0)
    gPlayerState . pHeading .= FaceRight 

stopMoveLeft :: (MonadRWS Environment [String] GameState m) => m () 
stopMoveLeft = do
    gPlayerState . pDirection .= (0, 0)

stopMoveRight :: (MonadRWS Environment [String] GameState m) => m () 
stopMoveRight = do
    gPlayerState . pDirection .= (0, 0)


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


