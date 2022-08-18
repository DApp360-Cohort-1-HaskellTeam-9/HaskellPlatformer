module Game.Init where

import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- initAssets
    -- ALUT
    -- sounds <- initSound
    -- ENDALUT
    return Environment
        { _eTileSize = 32
        , _eWindowWidth = 1024
        , _eWindowHeight = 768
        , _eFPS      = 360
        --, _eSounds  = sounds
        , _eAssets = assets
        -- ALUT
        -- , _eSounds  = sounds
        -- ENDALUT
        }

initState :: [String] -> ReaderT Environment IO GameState
initState args = do
    env <- ask
    return GameState
        { _gPlayerState   = initPlayer
        , _gLevelState    = runReader initLevel env
        , _gTotalKeys     = 3
        , _gDoorOpen      = False
        , _gTimeRemaining = 60
        , _gDeltaSec      = 0
        , _gSec           = 0
        , _gForce         = 10 -- gravity constant for this level
        , _gGameScene     = SceneStart
        , _gParallax      = (0, 0)
        , _gTransition    = 1
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
    , _pLives         = 3
    }

initLevel :: Reader Environment LevelState
initLevel = loadLevel minBound