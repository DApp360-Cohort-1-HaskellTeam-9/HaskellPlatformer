{-# LANGUAGE TemplateHaskell #-}

module Game.Data.Asset where

import Control.Lens
import Graphics.Gloss
import Game.Data.State

data Assets
    = Sprites -- Use Assets instead of Sprites?
    { _aPlayer   :: [Picture]
    , _aKey      :: (Picture, CellType)  -- collect keys instead of food
    , _aDoor     :: [Picture] -- locked | unlocked by collecting all keys
    , _aBase     :: Picture
    , _aGrass    :: Picture 
    , _aCoin     :: [Picture] -- Spinning coin sprites?
    , _aBgImg    :: [Picture] -- different bg image for each level?
    , _aTxtPause :: Picture
    , _aTxtTitle :: Picture
    , _aTxtEnter :: Picture
    , _aLvlNames :: [String] 
    , _aLvlFiles :: [String]
--  , we can expand as many as we need later
    }
makeLenses ''Assets
