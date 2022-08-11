{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

module Game.Data.Alias where

import Control.Monad.RWS

import Data.Kind

import Game.Data.Environment
import Game.Data.State

type family PureRWS (m :: Type -> Type) :: Constraint
type instance PureRWS m = MonadRWS Environment [String] GameState m

type RWSIO = RWST Environment [String] GameState IO
