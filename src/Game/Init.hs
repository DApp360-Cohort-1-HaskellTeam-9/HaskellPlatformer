module Game.Init where

import Control.Lens
import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- initAssets
    return Environment
        { _eTileSize = 32
        , _eFPS      = 360 -- on my screen, at 120 fps there's a noticable jitter on character move when using BMP sprite
                           -- my screen is only 144Hz, but there's a 360Hz gaming monitor on the market :-D
        , _eSprites  = assets
        }

initState :: [String] -> ReaderT Environment IO GameState
-- may need to take values from Environment?
initState args = do
    env <- ask
    level1 <- liftIO . readFile $ "./assets/levels/level1.txt"
    let level = runReader (prepareData . reverse . lines $ level1) env
    return GameState
        { _gCurrentLevel  = level
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
    
