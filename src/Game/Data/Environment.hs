{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Environment where

import Control.Lens

import Game.Data.Asset

data Environment
    = Environment 
    { _eWindowWidth  :: Int
    , _eWindowHeight :: Int
    , _eTileSize     :: Float -- All objects are same size, including player
    , _eFPS          :: Int -- frame rate
    , _eAssets       :: Assets
    -- ALUT
    -- , _eSounds       :: SoundInfo
    -- ENDALUT
--  , other configs, etc...
    }
makeLenses ''Environment
