{-# LANGUAGE TemplateHaskell #-}

module Game.Data.State where

import Control.Lens
import Game.Data.Enum
import Graphics.Gloss

type XY        = (Float, Float)
type CellType  = Char
type Cell      = (XY, CellType)
type GameLevel = [Cell]

data PlayerState
-- must declare PlayerState before GameState
-- due to lens
    = PlayerState
    { _pPosition       :: XY
    , _pSpeed          :: XY
    , _pIncSpeed       :: XY
    , _pMaxSpeed       :: XY
    , _pMovement       :: PlayerMovement
    , _pHeading        :: CharacterFacing
    , _pSpriteIndex    :: Float -- increment this using sec :: Float
    -- , _pScores         :: Int
    , _pCollectedKeys  :: Int
    , _pLives          :: Int
    }
makeLenses ''PlayerState

data EnemyState
    = EnemyState
    { _ePosition    :: XY
    , _eSpeed       :: XY
    , _eIncSpeed    :: XY
    , _eMaxSpeed    :: XY
    , _eHeading     :: CharacterFacing
    , _eSpriteIndex :: Float
    }
makeLenses ''EnemyState

data LevelState
    = LevelState
    { _lLevelName     :: LevelName -- Name of level
    , _lLevelCells    :: GameLevel -- Cell representation of level 
    }
makeLenses ''LevelState

data GameState
    = GameState 
    { _gPlayerState    :: PlayerState
    , _gEnemies        :: [EnemyState]
    , _gLevelState     :: LevelState
    , _gTotalKeys      :: Int
    , _gDoorOpen       :: Bool
    , _gTimeRemaining  :: Float  -- Time limit
    , _gDeltaSec       :: Float
    , _gSec            :: Float
    , _gForce          :: Float
    , _gGameScene      :: GameScene
    , _gTransition     :: Float
    , _gParallax        :: XY
    }
makeLenses ''GameState
