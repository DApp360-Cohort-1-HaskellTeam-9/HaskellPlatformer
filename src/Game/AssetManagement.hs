{-# LANGUAGE FlexibleContexts #-}

module Game.AssetManagement where

import Control.Lens
import Control.Monad.RWS

import Game.Data.Alias
import Game.Data.Asset
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State

import Data.Maybe

import Graphics.Gloss

--import Sound.ALUT as Sound

--loadImgs :: RWST Environment [String] GameState IO [Picture]

initAssets :: IO Assets
initAssets = do
    keyImg       <- loadBMP "./assets/graphics/items/key.bmp"
    txtPause     <- loadBMP "./assets/graphics/text/continue.bmp" 
    txtTitle     <- loadBMP "./assets/graphics/text/title.bmp" 
    txtEnter     <- loadBMP "./assets/graphics/text/enter.bmp"
    txtDigits    <- loadTxtDigits
    coinImgs     <- loadCoin
    doorImgs     <- loadDoor
    bgImgs       <- loadBackgrounds
    playerImgs   <- loadPlayers
    baseImgs     <- loadBaseTiles
    lvlData      <- loadLevels

    return Assets
        { _aPlayer     = playerImgs
        , _aKey        = (keyImg, 'k')
        , _aDoor       = doorImgs
        , _aBase       = last $ baseImgs -- TODO: Is there a better function?
        , _aGrass      = head $ baseImgs -- TODO: Is there a better function?
        , _aCoin       = coinImgs
        , _aBgImg      = bgImgs
        , _aTxtPause   = txtPause
        , _aTxtEnter   = txtEnter
        , _aTxtTitle   = txtTitle
        , _aTxtDigits  = txtDigits
        , _aLvlNames   = fst lvlData
        , _aLvlFiles   = snd lvlData
        }
    
{-
initSound :: IO SoundInfo
initSound = withProgNameAndArgs runALUTUsingCurrentContext $ \ _ _ -> do
    (Just device)  <- openDevice Nothing
    (Just context) <- createContext device []
    currentContext $= Just context
    
    let -- Credits to: dixonary / hake
        -- Load our sound file enum into an array.
        soundFiles :: [SoundType]
        soundFiles = [minBound..maxBound]
        
        soundPath :: SoundType -> String
        soundPath Coin      = "./assets/sounds/wizzle.wav"
        soundPath Key       = "./assets/sounds/pellet.wav"
        soundPath DoorOpen  = "./assets/sounds/file2.au"
        soundPath DoorClose = "./assets/sounds/blip.wav"
        
        -- Generate buffer queue for each sound.
        loadBuffer sf = do
            buf <- createBuffer $ File $ soundPath sf
            [src] <- genObjectNames 1
            queueBuffers src [buf]
            return (sf, src)
    
    -- Run loadBuffer for each soundFile.
    sounds <- mapM loadBuffer soundFiles
    
    -- Construct our stateful SoundInfo.
    return $ SoundInfo device context sounds
-}

rootDir :: String
rootDir = "./assets/graphics/"

loadPlayers :: IO [Picture]
loadPlayers = do
    let dir = rootDir ++ "characters/"
    let imgNames = map (\x -> "player" ++ x ++ "_35x45") ["Left1","Left2","Left3","Left4","Right1","Right2","Right3","Right4"]
    playerImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames -- Could also sequence to flip type
    return playerImgs

loadCoin :: IO [Picture]
loadCoin = do
    let dir = rootDir ++ "items/"
    let imgNames = ["coin"]
    coinImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return coinImgs

loadBaseTiles :: IO [Picture]
loadBaseTiles = do
    let dir = rootDir ++ "base/"
    let imgNames = ["grass", "base"]
    baseImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return baseImgs

loadDoor :: IO [Picture]
loadDoor = do
    let dir = rootDir ++ "door/"
    let imgNames = ["door_openMid", "door_openTop", "door_closedMid", "door_closedTop"]
    doorImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return doorImgs

loadTxtDigits :: IO [Picture]
loadTxtDigits = do
    let dir = rootDir ++ "text/"
    let imgNames = "0123456789"
    digitImgs <- mapM (loadBMP . (\n -> dir ++ n : ".bmp")) imgNames
    return digitImgs


--Add more backgrounds later
loadBackgrounds :: IO [Picture]
loadBackgrounds = do
    let dir = rootDir ++ "backgrounds/"
    let imgNames = ["Level1", "Level2","Level3", "LevelStart", "LevelCredits"] --Placeholder for 5 backgrounds
    bgImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return bgImgs

loadLevels :: IO ([String],[String])
loadLevels = do
    let dir = "assets/levels/"
    let lvlNames = ["Level1", "Level2","Level3"]
    levels <- mapM (readFile . (\n -> dir ++ n ++ ".txt")) lvlNames
    return $ (lvlNames ++ ["LevelStart", "LevelCredits"], levels)

incPlayerSprite :: (PureRWS m) => m ()
incPlayerSprite = do
    movement <- use (gPlayerState . pMovement)
    case movement of
        MoveStop -> return ()
        _        -> do
            delta <- use gDeltaSec
            gPlayerState . pSpriteIndex %= (+delta*10)
        
    

getPlayerSprite :: (PureRWS m) => m Picture
getPlayerSprite = do
    env <- ask
    
    let playerSprites    = view (eAssets . aPlayer) env
    let (lFaces, rFaces) = splitAt 4 playerSprites
    
    spriteIndex <- use (gPlayerState . pSpriteIndex)
    let i = truncate spriteIndex `mod` 4
    
    face <- use (gPlayerState . pHeading)
    return $ case face of
        FaceRight -> rFaces !! i
        FaceLeft  -> lFaces !! i
    

getDoorSprite :: (PureRWS m) => m (Picture, Picture)
getDoorSprite = do
    env <- ask
    isDoorOpen <- use gDoorOpen
    let doorImgs = (view (eAssets . aDoor) env)
    let (doorTopImg,doorBottomImg) = 
            case isDoorOpen of
                True  -> case   (doorImgs ^? element 1, doorImgs ^? element 0) of
                                (Just x, Just y)  -> (Just x, Just y)
                                (_,_)             -> (Nothing, Nothing)       
                False -> case   (doorImgs ^? element 3, doorImgs ^? element 2) of
                                (Just x, Just y)  -> (Just x, Just y)     
                                (_,_)             -> (Nothing, Nothing)   
    return (fromJust $ doorTopImg, fromJust $ doorBottomImg)

{-
playSound :: SoundType -> RWSIO ()
playSound s = do
    env <- ask
    let soundContext = view (eSounds . sContext) env
    let soundSources = view (eSounds . sSources) env
    withProgNameAndArgs runALUTUsingCurrentContext $ \ _ _ -> do
        currentContext $= Just soundContext
        Sound.play . maybeToList $ lookup s soundSources
-}

getCollidables :: [CellType] -- this is a list of collidables cell types
getCollidables = "*^" -- open to suggestions to improve this function :)

getCoinCellType :: [CellType]
getCoinCellType = "c"

getKeyCellType :: [CellType]
getKeyCellType = "k"

getDoorCellType :: [CellType]
getDoorCellType = "tb"

type Bounciness = Float
type BounceStop = Float
getBounciness :: CellType -> (Bounciness, BounceStop)
getBounciness c = case c of
    '^' -> (0.50, 0.05)
    '*' -> (0.25, 0.05)
    _   -> (0.00, 0.00)
