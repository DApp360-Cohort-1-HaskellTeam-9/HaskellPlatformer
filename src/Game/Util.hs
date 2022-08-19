module Game.Util where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.Writer

import Game.Data.Alias
import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

logDebug :: (PureRWS m) => String -> m ()
logDebug msg = do
    env <- ask
    level <- use (gLevelState . lLevelName)
    scene <- use gGameScene
    tell [show level ++ ": " ++ msg]
    --let doorImgs = view (eAssets . aDoor) env
    --tell ["Door image names" ++ (show doorImgs)]

-- | Create a GameLevel from a text file level representation
prepareData :: [String] -> Reader Environment GameLevel
prepareData [] = return []
prepareData (s:ss) = do
    env <- ask
    return $ -- rowNumber is counted from bottom
        runReader (makeRow s rowNumber) env ++
        runReader (prepareData ss     ) env
        where rowNumber = length ss
    

-- | Parse a row in a text level representation 
makeRow :: String -> Int -> Reader Environment GameLevel
makeRow [] _ = return []
makeRow (c:cs) rowNumber
    | c == '.'  = makeRow cs rowNumber  --- ^ Skip empty cell
    | otherwise = do
        env <- ask
        let (w, h) = (fromIntegral $ view eWindowWidth  env
                     ,fromIntegral $ view eWindowHeight env)
            row    = fromIntegral $ rowNumber
            col    = fromIntegral $ length cs --- ^ Column number is counted from right to left
            tile   = view eTileSize env
            xPos   = w / 2 - tile / 2 - col * tile
            yPos   = h / 2 - tile / 2 - row * tile
        return $ ((xPos, yPos), c) : runReader (makeRow cs rowNumber) env
    

levelItemCount :: GameLevel -> [CellType] -> Int
levelItemCount level items =
    length $ filter isItem level
    where isItem = (`elem` items) . snd

isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize =
    x1            < x2 + tileSize &&
    x1 + tileSize > x2            &&
    y1            < y2 + tileSize &&
    y1 + tileSize > y2
