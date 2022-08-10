module Game.Init where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Data.Asset

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- initAssets
    return Environment
        { _eTileSize = 32
        , _eFPS      = 360
        , _eSprites  = assets
        }

initState :: [String] -> ReaderT Environment IO GameState
initState args = do
    env <- ask
    let level1 = view (eSprites . aLevels) env
    let levelCells = runReader (prepareData . reverse . lines $ (head level1)) env

    return GameState
        { _gCurrentLevel  = levelCells
        , _gLevelName     = head level1  -- names should correspond to the name of the text values in aLevels
        , _gPlayerState   = initPlayer
        , _gTotalKeys     = 3
        , _gDoorOpen      = False
        , _gPaused        = False
        , _gTimeRemaining = 120
        , _gDeltaSec      = 0
        , _gForce         = 10 -- gravity constant for this level
        }
    

initPlayer :: PlayerState
initPlayer = PlayerState
    { _pPosition      = (0, 0)
    , _pSpeed         = (0, 0)
    , _pIncSpeed      = (5000, 1000) -- need playtests
    , _pMaxSpeed      = (500, -1000) -- to tweak these
    , _pMovement      = MoveStop
    , _pHeading       = FaceRight
    , _pSpriteIndex   = 0
    , _pCollectedKeys = 0
    }

prepareData :: [String] -> Reader Environment GameLevel
prepareData rawData = do
    env <- ask
    return . concat $
        [runReader (makeRow (rawData !! y) y) env | y <- [0 .. length rawData - 1]]
    

makeRow :: String -> Int -> Reader Environment GameLevel
makeRow row y = do
    env <- ask
    let tileSize = view eTileSize env
    return
        [ (((fromIntegral x * tileSize) - ((1024 / 2) - (tileSize / 2))
        , (fromIntegral y * tileSize) - ((768 / 2) - (tileSize / 2)))
        , row !! x) -- TODO: get rid of partial functions and list comprehensions
        | x <- [0 .. length row - 1]
        , row !! x /= '.'
        ]
    
