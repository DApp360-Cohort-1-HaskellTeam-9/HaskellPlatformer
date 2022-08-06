{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Asset where

import Control.Lens
import Graphics.Gloss

data Assets
    = Sprites -- Use Assets instead of Sprites?
    { _aPlayer    :: [Picture]
    , _aKey       ::  Picture  -- collect keys instead of food
    , _aDoor      :: [Picture] -- locked | unlocked by collecting all keys
    , _aBaseTiles :: [Picture] 
    , _aCoin      ::  Picture -- If required. Spinning coin? 
--  , we can expand as many as we need later
    }
makeLenses ''Assets
