{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Asset where

import Control.Lens
import Graphics.Gloss
import Game.Data.State

--import Sound.ALUT as Sound

data Assets
    = Assets
    { _aPlayer  :: [Picture]
    , _aKey     :: (Picture, CellType)  -- collect keys instead of food
    , _aDoor    :: [Picture] -- locked | unlocked by collecting all keys
    , _aBase    :: Picture
    , _aGrass   :: Picture 
    , _aCoin    :: [Picture] -- Spinning coin sprites?
    , _aBgImg   :: [Picture] -- different bg image for each level?
    , _aTxtCont :: Picture
    , _aLevels  :: [String] 
--  , we can expand as many as we need later
    }
makeLenses ''Assets

-- must be declared in this particular order due to Lens
--data SoundType
--    = Coin
--    | Key
--    | DoorOpen
--    | DoorClose
--    deriving (Bounded, Enum, Eq)
--data SoundInfo
--    = SoundInfo
--    { _sDevice  :: Sound.Device
--    , _sContext :: Sound.Context
--    , _sSources :: [(SoundType, Sound.Source)]
--    }
--makeLenses ''SoundInfo
