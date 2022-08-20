module Game.Input where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Init
import Game.Logic
import Game.Util

import Graphics.Gloss.Interface.IO.Game

import System.Exit

handleKeys :: Event -> RWSIO GameState
handleKeys e = do
    scene <- use gGameScene
    case (scene, e) of
        (SceneStart  , (EventKey (SpecialKey KeyEnter) Up   _ _)) -> beginGame
        (SceneLevel  , (EventKey (SpecialKey KeyLeft ) Down _ _)) -> moveLeft
        (SceneLevel  , (EventKey (SpecialKey KeyRight) Down _ _)) -> moveRight
        (SceneLevel  , (EventKey (SpecialKey KeyUp   ) Down _ _)) -> moveUp
        (SceneLevel  , (EventKey (SpecialKey KeyLeft ) Up   _ _)) -> stopMoveLeft
        (SceneLevel  , (EventKey (SpecialKey KeyRight) Up   _ _)) -> stopMoveRight
        (SceneLevel  , (EventKey (Char       'p'     ) Up   _ _)) -> pauseGame
        (ScenePause  , (EventKey (Char       'p'     ) Up   _ _)) -> unpauseGame
        (SceneLose   , (EventKey (SpecialKey KeyEnter) Up   _ _)) -> resetGame
        (SceneCredits, (EventKey (SpecialKey KeyEnter) Up   _ _)) -> resetGame
        (_           , (EventKey (SpecialKey KeyEsc  ) Up   _ _)) -> quitGame
        _ -> return ()
    get -- return GameState

beginGame :: RWSIO ()
beginGame = do
    gGameScene   .= SceneLevel
    gPlayerState .= initPlayer -- reset player
    
    enemies  <- initEnemies
    gEnemies .= enemies -- spawn enemies

moveLeft :: (PureRWS m) => m ()
moveLeft = do
    gPlayerState . pMovement .= MoveLeft
    gPlayerState . pHeading  .= FaceLeft

moveRight :: (PureRWS m) => m () 
moveRight = do
    gPlayerState . pMovement .= MoveRight
    gPlayerState . pHeading  .= FaceRight

moveUp :: (PureRWS m) => m ()
moveUp = do
    env <- ask
    let tileSize = view eTileSize env
    
    (x, y) <- use (gPlayerState . pPosition)
    let colliders = getCollidables
    
    hit <- collideWith colliders (x, y - tileSize)
    case hit of
        Just _ -> do
            (currSpeedX, _) <- use (gPlayerState . pSpeed)
            gPlayerState . pSpeed .= (currSpeedX , 2000)
        Nothing -> return ()
    

stopMoveLeft :: (PureRWS m) => m () 
stopMoveLeft = do
    movement <- use (gPlayerState . pMovement)
    case movement of
        MoveLeft -> gPlayerState . pMovement .= MoveStop
        _        -> return ()
    

stopMoveRight :: (PureRWS m) => m () 
stopMoveRight = do
    movement <- use (gPlayerState . pMovement)
    case movement of
        MoveRight -> gPlayerState . pMovement .= MoveStop
        _         -> return ()
    

pauseGame :: (PureRWS m) => m ()
pauseGame = do
    gGameScene .= ScenePause
    heading <- use (gPlayerState . pHeading)
    case heading of
        FaceRight -> stopMoveRight
        FaceLeft  -> stopMoveLeft
    

unpauseGame :: (PureRWS m) => m ()
unpauseGame = gGameScene .= SceneLevel

resetGame :: RWSIO ()
resetGame = do
    env   <- ask
    resetGame <- liftIO $ runReaderT (initState []) env
    put resetGame

quitGame :: RWSIO ()
quitGame = do
    -- logDebug "Goodbye"
    liftIO exitSuccess
