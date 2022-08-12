{-# LANGUAGE TemplateHaskell #-}

module Game.Data.State where

import Control.Lens

import Game.Data.Enum

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
    , _pHeading        :: PlayerFacing
    , _pSpriteIndex    :: Float -- increment this using sec :: Float
    , _pCollectedKeys  :: Int
    }
makeLenses ''PlayerState

data LevelState
    = LevelState
    { _lLevelName      :: LevelName    -- Name of level
    , _lLevelCells     :: GameLevel    -- Cell representation of level 
    }
makeLenses ''LevelState

data GameState
    = GameState 
    { _gPlayerState    :: PlayerState
    , _gLevelState     :: LevelState
    , _gTotalKeys      :: Int
    --, _gPaused         :: Bool
    , _gDoorOpen       :: Bool
    , _gTimeRemaining  :: Float  -- Time limit
    , _gDeltaSec       :: Float
    , _gForce          :: Float
    , _gGameScene      :: GameScene
    }
makeLenses ''GameState


