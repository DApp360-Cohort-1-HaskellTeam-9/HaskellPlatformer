{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Environment where

import Control.Lens

import Game.Data.Asset

data Environment
    = Environment -- proposed new Environment
    { _eWindowWidth  :: Int
    , _eWindowHeight :: Int
    , _eTileSize     :: Float -- All objects are same size, including player
    , _eFPS          :: Int -- frame rate
    , _eSprites      :: Assets
--  , _eSounds       :: SoundInfo
--  , other configs, etc...
    }
makeLenses ''Environment
