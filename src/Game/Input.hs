module Game.Input where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss.Interface.IO.Game

handleKeys :: Event -> RWST Environment [String] GameState IO GameState
handleKeys = undefined

{-
handleKeys :: Event -> GameState -> IO GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
    return $ gs {direction = West, heading = FacingWest}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
    return $ gs {direction = East, heading = FacingEast}
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gs =
    return $ gs
    { speedY = 6
        -- if isCollision gs (fst (position gs), snd (position gs) + speedY gs) '*'
        --     then 6
        --     else speedY gs -- (-4)
    }
handleKeys (EventKey (SpecialKey KeyLeft)  Up _ _) gs =
    return $ case direction gs of
        West -> gs { direction = None }
        _    -> gs
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gs =
    return $ case direction gs of
        East -> gs { direction = None }
        _    -> gs
handleKeys _ gs = return gs
-}

-- exitGame??
-- pauseGame??