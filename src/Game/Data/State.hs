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
    { _pPosition      :: XY
    , _pSpeed         :: XY
    , _pIncSpeed      :: XY
    , _pMaxSpeed      :: XY
    , _pMovement      :: PlayerMovement
    , _pHeading       :: PlayerFacing
    , _pSpriteIndex   :: Float -- increment this using sec :: Float
    , _pCollectedKeys :: Int
--  , _pJumpCounter, etc...
    }
makeLenses ''PlayerState

data GameState
    = GameState -- new proposed GameState
    { _gCurrentLevel  :: GameLevel --This could include the gameover/highscore "level"
    , _gPlayerState   :: PlayerState
    , _gTotalKeys     :: Int
    , _gPaused        :: Bool
    , _gDoorOpen      :: Bool
    , _gTimeRemaining :: Int  -- Time limit
    , _gDeltaSec      :: Float
    , _gForce         :: Float
    }
makeLenses ''GameState
