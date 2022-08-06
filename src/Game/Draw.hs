module Game.Draw where

import Control.Monad.RWS

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss


renderGame :: RWST Environment [String] GameState IO Picture
renderGame = undefined
{-
renderGame gs imgs = do
    return $ 
     (pictures $ [translate 0 0 (imgs !! 6)] ++ (map (\ cell -> renderTile cell imgs) (currentLevel gs) --Tiles
     ++ 
     [translate (fst (position gs)) (snd (position gs)) (imgs !! (spriteCount gs + 6 + isRight (heading gs)))])) 
-}


updateGame :: Float -> RWST Environment [String] GameState IO GameState
updateGame = undefined
{-
updateGame sec = do
    gs <- lift get -- TODO: Lens
    tileSize <- tileSize <$> ask
    env <- ask
    return gs
        { speedY = runReader (updateSpeedY gs) env
        , speedX = updateSpeedX gs
        , position = runReader (moveY gs $ runReader (moveX (direction gs) gs) env) env  -- utilize `sec`
        , spriteCount = incSprite gs -- use Sequence?
        , currentLevel = runReader (removeItem gs) env --currentLevel = runReader (removeItem gs) env
        }
-}

-- Helper Functions:

--REPLACE !! with LENS?
renderTile :: Cell -> [Picture] -> Picture
renderTile (pos, cellType) imgs =
    let baseImg = imgs !! 0
        grassImg = imgs !! 1
        coinImg = imgs !! 2
        keyImg = imgs !! 3
        doorCTImg = imgs !! 4
        doorCMImg = imgs !! 5
    in
    uncurry translate (pos) $
    case cellType of
     '*' -> baseImg 
     'a' -> grassImg
     '%' -> coinImg 
     'k' -> keyImg
     't' -> doorCTImg
     'b' -> doorCMImg

checkImg :: undefined
checkImg = undefined

{-
--Enemies to appear at random times
renderEnemy :: undefined
renderEnemy = undefined
-}