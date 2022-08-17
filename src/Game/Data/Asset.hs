{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Asset where

import Control.Lens

import Game.Data.Enum
import Game.Data.State

import Graphics.Gloss

-- import Sound.ALUT as Sound

data Assets
    = Assets -- Use Assets instead of Sprites?
    { _aPlayer       :: [Picture]
    , _aKey          :: (Picture, CellType)  -- collect keys instead of food
    , _aDoor         :: [(Int, Picture)] -- locked | unlocked by collecting all keys
    , _aBase         :: Picture
    , _aGrass        :: Picture 
    , _aCoin         :: [Picture] -- Spinning coin sprites?
    , _aBgImg        :: [Picture] -- different bg image for each level?
    , _aTxtPause     :: Picture
    , _aTxtTitle     :: Picture
    , _aTxtEnter     :: Picture
    , _aTxtContinue  :: Picture
    , _aTxtCredits   :: Picture
    , _aTxtGameover  :: Picture
    , _aTxtDigits    :: [Picture]
    , _aLvlNames     :: [String] 
    , _aLvlFiles     :: [String]
    , _aLvlTitles    :: [(LevelName, Picture)]
    , _aLvlSubtitles :: [(LevelName, Picture)]
    }
makeLenses ''Assets

-- must be declared in this particular order due to Lens
data SoundType
    = Coin
    | Key
    | DoorOpen
    | DoorClose
    deriving (Bounded, Enum, Eq)
-- ALUT
-- data SoundInfo
--     = SoundInfo
--     { _sDevice  :: Sound.Device
--     , _sContext :: Sound.Context
--     , _sSources :: [(SoundType, Sound.Source)]
--     }
-- makeLenses ''SoundInfo
-- ENDALUT
