module Game.Util where

import Control.Lens
import Control.Monad.Reader

import Game.Data.Environment
import Game.Data.State

import Graphics.Gloss

-- | Create a GameLevel from a text file level representation
prepareData :: [String] -> Reader Environment GameLevel
prepareData [] = return []
prepareData (s:ss) = do
    env <- ask 
    return $ concat [runReader (makeRow s rowNumber) env, runReader (prepareData ss) env]
        where rowNumber = length ss    --- ^ Row number is counted from bottom
    

-- | Parse a row in a text level representation 
makeRow :: String -> Int -> Reader Environment GameLevel
makeRow [] _ = return []
makeRow (c:cs) rowNumber
    | c == '.'  = makeRow cs rowNumber  --- ^ Skip empty cell
    | otherwise = do
        env <- ask
        let windowWidth  = view eWindowWidth env
        let windowHeight = view eWindowHeight env
        let tileSize     = view eTileSize env
        let colNumber = length cs    --- ^ Column number is counted from right to left
        let xPos = fromIntegral windowWidth / 2  - tileSize / 2 - fromIntegral colNumber * tileSize
        let yPos = fromIntegral windowHeight / 2 - tileSize / 2 - fromIntegral rowNumber * tileSize
        return $ ((xPos, yPos), c) : runReader (makeRow cs rowNumber) env
    

levelItemCount :: GameLevel -> [CellType] -> Int
levelItemCount level items =
    length $ filter ((`elem` items) . snd) level

isHit :: Point -> Point -> Float -> Bool
isHit (x1, y1) (x2, y2) tileSize =
    x1            < x2 + tileSize &&
    x1 + tileSize > x2            &&
    y1            < y2 + tileSize &&
    y1 + tileSize > y2
