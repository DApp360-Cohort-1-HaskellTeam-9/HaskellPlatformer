module Game.Init where

import Control.Lens
import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

import System.Random

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- initAssets
    -- ALUT
    sounds <- initSound
    -- ENDALUT
    return Environment
        { _eTileSize = 32
        , _eWindowWidth = 1024
        , _eWindowHeight = 768
        , _eFPS      = 360
        , _eAssets = assets
        -- ALUT
        , _eSounds  = sounds
        -- ENDALUT
        }

initState :: [String] -> ReaderT Environment IO GameState
initState args = do
    env <- ask
    return GameState
        { _gPlayerState   = initPlayer
        , _gEnemies       = []
        , _gLevelState    = runReader initLevel env
        , _gTotalKeys     = 3
        , _gDoorOpen      = False
        , _gTimeRemaining = 120
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
    , _pScores        = 0
    , _pCollectedKeys = 0
    , _pLives         = 3
    }

initEnemies :: RWSIO [EnemyState]
initEnemies = do
    level <- use (gLevelState . lLevelCells)
    let enemyCells = filter ((`elem` getEnemyCellType) . snd) level
    enemies <- forM enemyCells (\ (pos,_) -> do
        rng <- randomRIO (4 :: Int, 7)
        return EnemyState
            { _ePosition    = pos
            , _eSpeed       = (0, 0)
            , _eIncSpeed    = (2000, 1000) -- just copy player stats
            , _eMaxSpeed    = (20 * fromIntegral rng, -1000)
            , _eHeading     = if odd rng then FaceLeft else FaceRight
            , _eSpriteIndex = fromIntegral rng
            })
    return enemies

initLevel :: Reader Environment LevelState
initLevel = loadLevel minBound
