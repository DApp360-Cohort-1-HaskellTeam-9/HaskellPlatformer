{-# LANGUAGE FlexibleContexts #-}

module Game.Input where

import Control.Monad.RWS
import Control.Lens

import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss.Interface.IO.Game

--TODO: Add text "Press 'p' to continue" or similar. t

handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys e = do
    isPaused <- use gPaused
    heading  <- use (gPlayerState . pHeading)
    case e of
        (EventKey (Char 'p') Down _ _) -> do
            pauseGame
            if heading == FaceLeft then stopMoveLeft else stopMoveRight
        _                              -> 
            case isPaused of
                True -> return ()
                False -> case e of 
                        (EventKey (SpecialKey KeyLeft) Down _ _)  -> 
                            moveLeft
                        (EventKey (SpecialKey KeyRight) Down _ _) -> 
                            moveRight
                        (EventKey (SpecialKey KeyUp) Down _ _)    -> 
                            moveUp
                        (EventKey (SpecialKey KeyLeft) Up _ _)    -> 
                            stopMoveLeft
                        (EventKey (SpecialKey KeyRight) Up _ _)   -> 
                            stopMoveRight
                        _                                         ->
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

-- exitGame??


