{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Asset where

import Control.Lens
import Graphics.Gloss

data Assets
    = Sprites
    { _aPlayer :: [Picture]
    , _aKey    ::  Picture  -- collect keys instead of food
    , _aDoor   :: [Picture] -- locked | unlocked by collecting all keys
    , _aTile   ::  Picture
--  , we can expand as many as we need later
    }
makeLenses ''Assets
