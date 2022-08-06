module Game.Init where

import Control.Lens
import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- loadAssets
    return Environment
        { _eTileSize = 32
        , _eFPS      = 360 -- smoother on monitors with higher framerate
        , _eSprites  = assets
        }

initState :: [String] -> ReaderT Environment IO GameState
-- may need to take values from Environment?
initState args = do
    env <- ask
    level1 <- liftIO . readFile $ "./assets/levels/1.txt"
    let level = runReader (prepareData . reverse . lines $ level1) env
    return GameState
        { _gCurrentLevel = level
        , _gPlayerState  = initPlayer
        , _gTotalKeys    = 19
        , _gDoorOpen     = False
    --  , etc...
        , _gDeltaSec     = 0
        , _gSec          = 0
        }
    

initPlayer :: PlayerState
initPlayer = PlayerState
    { _pPosition      = (0, 0)
    , _pSpeed         = (0, -5)
    , _pDirection     = (0, 0)
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
        , row !! x == '*' || row !! x == '%'
        ]
    
