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
    , _pDirection     :: XY -- (range from -1 to 1, range from -1 to 1)
    , _pHeading       :: PlayerFacing
    , _pSpriteIndex   :: Float -- increment this using sec :: Float
    , _pCollectedKeys :: Int
--  , _pJumpCounter, etc...
    }
makeLenses ''PlayerState

--Add door position? Need to know for collision to trigger next level
data GameState
    = GameState -- new proposed GameState
    { _gCurrentLevel  :: GameLevel --This could include the gameover/highscore "level"
    , _gPlayerState   :: PlayerState
    , _gTotalKeys     :: Int
    , _gDoorOpen      :: Bool
    , _gTimeRemaining :: Int  -- Time limit
    --  , etc...
    , _gDeltaSec     :: Float
    , _gSec          :: Float
    }
makeLenses ''GameState
