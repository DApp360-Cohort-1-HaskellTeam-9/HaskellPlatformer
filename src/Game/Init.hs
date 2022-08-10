module Game.Init where

import Control.Lens
import Control.Monad.Reader

import Game.AssetManagement
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Data.Asset

import Sound.ALUT

initEnv :: [String] -> IO Environment
initEnv args = do
    assets <- initAssets
    return Environment
        { _eTileSize = 32
        , _eWindowWidth = 1024
        , _eWindowHeight = 768
        , _eFPS      = 360 -- on my screen, at 120 fps there's a noticable jitter on character move when using BMP sprite
                           -- my screen is only 144Hz, but there's a 360Hz gaming monitor on the market :-D
        , _eSprites  = assets
        }
    

initState :: [String] -> ReaderT Environment IO GameState
initState args = do
    env <- ask
    let level1 = head $ view (eSprites . aLevels) env
    let levelCells = runReader (prepareData . reverse . lines $ level1) env

    withProgNameAndArgs runALUT $ \_progName _args -> do
        introBuffer <- createBuffer (File "./assets/sounds/file2.au")
        introSource <- genObjectName
        buffer introSource $= Just introBuffer
        play [introSource]
        sleep 1
    
    return GameState
        { _gCurrentLevel  = levelCells
        , _gLevelName     = level1  -- names should correspond to the name of the text values in aLevels
        , _gPlayerState   = initPlayer
        , _gTotalKeys     = 3
        , _gDoorOpen      = False
        , _gPaused        = False
        , _gTimeRemaining = 120
        , _gDeltaSec      = 0
        , _gForce         = 10 -- gravity constant for this level
        }
    

initPlayer :: PlayerState
initPlayer = PlayerState
    { _pPosition      = (0, 0)
    , _pSpeed         = (0, 0)
    , _pIncSpeed      = (5000, 1000) -- need playtests
    , _pMaxSpeed      = (500, -1000) -- to tweak these
    , _pBounciness    = 0.5
    , _pBounceCutoff  = 0.1
    , _pMovement      = MoveStop
    , _pHeading       = FaceRight
    , _pSpriteIndex   = 0
    , _pCollectedKeys = 0
    }

-- | Parse a row in a text level representation 
makeRow :: String -> Int -> Reader Environment GameLevel
makeRow [] _ = return []
makeRow (c:cs) rowNumber
    | c == '.'  = makeRow cs rowNumber  -- ^ Skip empty cell
    | otherwise = do
        env <- ask
        let windowWidth  = view eWindowWidth env
        let windowHeight = view eWindowHeight env
        let tileSize     = view eTileSize env
        let colNumber = length cs    -- ^ Column number is counted from right to left
        let xPos = fromIntegral windowWidth / 2  - tileSize / 2 - fromIntegral colNumber * tileSize
        let yPos = fromIntegral windowHeight / 2 - tileSize / 2 - fromIntegral rowNumber * tileSize
        return $ ((xPos, yPos), c) : runReader (makeRow cs rowNumber) env
    

-- | Create a GameLevel from a text file level representation
prepareData :: [String] -> Reader Environment GameLevel
prepareData [] = return []
prepareData (s:ss) = do
    env <- ask 
    return $ concat [runReader (makeRow s rowNumber) env, runReader (prepareData ss) env]
        where rowNumber = length ss    -- ^ Row number is counted from bottom
    
