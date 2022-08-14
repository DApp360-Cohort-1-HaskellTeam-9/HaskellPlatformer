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
        , _eFPS      = 360 -- on my screen, at 120 fps there's a noticable jitter on character move when using BMP sprite
                           -- my screen is only 144Hz, but there's a 360Hz gaming monitor on the market :-D
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
        --, _gPaused        = False
        , _gTimeRemaining = 120
        , _gDeltaSec      = 0
        , _gForce         = 10 -- gravity constant for this level
        , _gGameScene     = SceneLevel
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
    }

initLevel :: Reader Environment LevelState
initLevel = loadLevel minBound
