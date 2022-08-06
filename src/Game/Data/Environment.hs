{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Environment where

import Control.Lens

import Game.Data.Asset
import Game.Data.State

data Environment
    = Environment -- proposed new Environment
    { _eTileSize  :: Float -- All objects are same size, including player
    , _eFPS       :: Int -- frame rate
    , _eSprites   :: Assets
    , _eBaseTiles :: [CellType] -- character representations of items
    , _eItemTiles :: [CellType] -- important to differentiate for collision, etc
--  , other configs, etc...
    }
makeLenses ''Environment
