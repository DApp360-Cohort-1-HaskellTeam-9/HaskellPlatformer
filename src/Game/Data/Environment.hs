{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Environment where

import Control.Lens

import Game.Data.Asset

data Environment
    = Environment -- proposed new Environment
    { _eTileSize :: Float
    , _eFPS      :: Int -- frame rate
    , _eSprites  :: Assets
--  , other configs, etc...
    }
makeLenses ''Environment
