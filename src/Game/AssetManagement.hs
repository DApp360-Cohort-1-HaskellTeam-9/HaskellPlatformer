module Game.AssetManagement where

import Control.Lens
import Control.Monad.Reader

import Game.Data.Alias
import Game.Data.Asset
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Util

import Data.Maybe

import Graphics.Gloss

import Sound.ALUT as Sound

initAssets :: IO Assets
initAssets = do
    heartImg     <- loadBMP "./assets/graphics/items/heartSmall.bmp"
    heartSmall   <- scale 0.5 0.5 <$> loadBMP "./assets/graphics/items/heartSmall.bmp"
    spikes       <- loadBMP "./assets/graphics/items/spikes.bmp"
    keyImg       <- loadBMP "./assets/graphics/items/key.bmp"
    txtPause     <- loadBMP "./assets/graphics/text/continue.bmp" 
    txtTitle     <- loadBMP "./assets/graphics/text/title.bmp" 
    txtEnter     <- loadBMP "./assets/graphics/text/enter.bmp"
    txtCredits   <- loadBMP "./assets/graphics/text/credits.bmp"
    txtGameover  <- loadBMP "./assets/graphics/text/gameover.bmp"
    txtContinue  <- loadBMP "./assets/graphics/text/enterContinue.bmp"
    txtDigits    <- loadTxtDigits
    coinImgs     <- loadCoin
    doorImgs     <- loadDoor
    bgImgs       <- loadBackgrounds
    playerImgs   <- loadPlayers
    enemyImgs    <- loadEnemies
    baseImgs     <- loadBaseTiles
    lvlData      <- loadLevels
    lvlTitles    <- loadLevelTransition ""
    lvlSubtitles <- loadLevelTransition "subtitle"
    return Assets
        { _aPlayer       = playerImgs
        , _aEnemy        = enemyImgs
        , _aKey          = (keyImg, 'k')
        , _aDoor         = doorImgs
        , _aBase         = last baseImgs -- TODO: Is there a better function?
        , _aGrass        = head baseImgs -- TODO: Is there a better function?
        , _aCoin         = coinImgs
        , _aHeart        = heartImg
        , _aHeartSmall   = heartSmall
        , _aSpikes       = spikes
        , _aBgImg        = bgImgs
        , _aTxtPause     = txtPause
        , _aTxtEnter     = txtEnter
        , _aTxtContinue  = txtContinue
        , _aTxtCredits   = txtCredits
        , _aTxtGameover  = txtGameover
        , _aTxtTitle     = txtTitle
        , _aTxtDigits    = txtDigits
        , _aLvlNames     = fst lvlData
        , _aLvlFiles     = snd lvlData
        , _aLvlTitles    = lvlTitles
        , _aLvlSubtitles = lvlSubtitles
        }
    

-- ALUT
initSound :: IO SoundInfo
initSound = withProgNameAndArgs runALUTUsingCurrentContext $ \ _ _ -> do
    Just device    <- openDevice Nothing
    Just context   <- createContext device []
    currentContext $= Just context
    
    let -- Credits to: dixonary / hake
        soundTypes :: [SoundType]
        soundTypes = [minBound..maxBound]
        
        soundPath :: SoundType -> String
        soundPath Coin      = "./assets/sounds/wizzle.wav"
        soundPath Key       = "./assets/sounds/pellet.wav"
        soundPath Hurt      = "./assets/sounds/die.wav"
        -- soundPath Hurt      = "./assets/sounds/file3.raw"
        soundPath TimeUp    = "./assets/sounds/file3.raw"
        soundPath Life      = "./assets/sounds/file1.wav"
        soundPath DoorOpen  = "./assets/sounds/file2.au"
        soundPath DoorClose = "./assets/sounds/blip.wav"
        
        -- Generate buffer queue for each sound.
        loadBuffer s = do
            buf   <- createBuffer $ File $ soundPath s
            [src] <- genObjectNames 1
            queueBuffers src [buf]
            return (s, src)
    
    -- Run loadBuffer for each soundFile.
    sounds <- forM soundTypes loadBuffer
    
    -- Construct our stateful SoundInfo.
    return $ SoundInfo device context sounds
-- ENDALUT

rootDir :: String
rootDir = "./assets/graphics/"

loadPlayers :: IO [Picture]
loadPlayers = do
    let dir = rootDir ++ "characters/"
        imgNames = map (\x -> "player" ++ x ++ "_35x45") ["Left1","Left2","Left3","Left4","Right1","Right2","Right3","Right4"]
    playerImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames -- Could also sequence to flip type
    return playerImgs

loadEnemies :: IO [Picture]
loadEnemies = do
    let dir = rootDir ++ "characters/enemy/"
        lefts = map (\ i -> dir ++ "enemyLeft"  ++ show i ++ ".bmp") [1..4]
        right = map (\ i -> dir ++ "enemyRight" ++ show i ++ ".bmp") [1..4]
    enemyImg <- mapM (loadBMP) $ lefts ++ right
    return enemyImg

loadCoin :: IO [Picture]
loadCoin = do
    let dir = rootDir ++ "items/"
        imgNames = ["coin"]
    coinImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return coinImgs

loadBaseTiles :: IO [Picture]
loadBaseTiles = do
    let dir = rootDir ++ "base/"
        imgNames = ["grass", "base"]
    baseImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return baseImgs

loadDoor :: IO [(Int, Picture)]
loadDoor = do
    let dir = rootDir ++ "door/"
        imgNames = ["door_openMid", "door_openTop", "door_closedMid", "door_closedTop"]
        imgKey = [0..]
    doorImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    let doorList = zip imgKey doorImgs
    return doorList

loadTxtDigits :: IO [Picture]
loadTxtDigits = do
    let dir = rootDir ++ "text/"
        imgNames = "0123456789"
    digitImgs <- mapM (loadBMP . (\n -> dir ++ n : ".bmp")) imgNames
    return digitImgs

loadBackgrounds :: IO [Picture]
loadBackgrounds = do
    let dir = rootDir ++ "backgrounds/"
        imgNames = map show [Level1 ..]-- Placeholder for all backgrounds
    bgImgs <- mapM (loadBMP . (\n -> dir ++ n ++ ".bmp")) imgNames
    return bgImgs

loadLevels :: IO ([String],[String])
loadLevels = do
    let levelDir = "./assets/levels/"
        lvlNames = map show [Level1, Level2, Level3] --[Level1 .. Level3]
        miscLvls = map show [LevelCredits, LevelStart]
    levels <- mapM (readFile . (\n -> levelDir ++ n ++ ".txt")) lvlNames
    return (lvlNames ++ miscLvls, levels)

loadLevel :: LevelName -> Reader Environment LevelState
loadLevel levelName = do
    env <- ask
    let levelNames    = view (eAssets . aLvlNames) env
        levelFiles    = view (eAssets . aLvlFiles) env
        lvlNameFile   = zip levelNames levelFiles
        levelFile     = fromMaybe "     " . (`lookup` lvlNameFile) $ show levelName
        levelCells    = (`runReader` env) . prepareData . reverse $ lines levelFile
    return LevelState
        { _lLevelName     = levelName
        , _lLevelCells    = levelCells
        }

loadLevelTransition :: String -> IO [(LevelName, Picture)]
loadLevelTransition asset = do
    let textPath = rootDir ++ "text/"
        lvlNames = map show [Level1 .. Level3]
    titles <- forM lvlNames (\ lv -> loadBMP $
        textPath ++ lv ++ asset ++ ".bmp")
    return $ zip [Level1 ..] titles

incPlayerSprite :: (PureRWS m) => m ()
incPlayerSprite = do
    movement <- use (gPlayerState . pMovement)
    case movement of
        MoveStop -> return ()
        _        -> do
            delta <- use gDeltaSec
            gPlayerState . pSpriteIndex %= (+delta*10)
        
    

incEnemiesSprites :: (PureRWS m) => m ()
incEnemiesSprites = do
    delta      <- use gDeltaSec
    currStates <- use gEnemies
    nextStates <- forM currStates (\ enemy -> do
        let (speed,_) = view eSpeed  enemy
            currIndex = view eSpriteIndex enemy
            nextIndex = currIndex + delta * speed / 5
        return enemy { _eSpriteIndex = nextIndex })
    gEnemies .= nextStates

getPlayerSprite :: (PureRWS m) => m Picture
getPlayerSprite = do
    env <- ask
    
    let playerSprites    = view (eAssets . aPlayer) env
        (lFaces, rFaces) = splitAt 4 playerSprites
    
    spriteIndex <- use (gPlayerState . pSpriteIndex)
    let i = truncate spriteIndex `mod` 4
    
    face <- use (gPlayerState . pHeading)
    return $ case face of
        FaceRight -> rFaces !! i
        FaceLeft  -> lFaces !! i
    

getEnemiesSprites :: (PureRWS m) => m [Picture]
getEnemiesSprites = do
    env <- ask
    enemies <- use gEnemies
    let enemySprites     = view (eAssets . aEnemy) env
        (lFaces, rFaces) = splitAt 4 enemySprites
    sprites <- forM enemies (\ enemy -> do
        let (x, y) = view ePosition    enemy
            facing = view eHeading     enemy
            sIndex = view eSpriteIndex enemy
            i = truncate sIndex `mod` 4
            f = case facing of
                FaceRight -> rFaces !! i
                FaceLeft  -> lFaces !! i
        return $ translate x (y-4) f)
    return sprites

getDoorSprite :: (PureRWS m) => m (Picture, Picture)
getDoorSprite = do
    env <- ask
    isDoorOpen <- use gDoorOpen
    let doorImgs = (view (eAssets . aDoor) env)
        (doorTopImg,doorBottomImg) = 
            case isDoorOpen of
                True -> case (lookup 1 doorImgs, lookup 0 doorImgs) of
                    (Just x, Just y)  -> (x, y)
                    _                 -> (blank, blank)
                False -> case (lookup 3 doorImgs, lookup 2 doorImgs) of
                    (Just x, Just y)  -> (x, y) 
                    (Nothing,Nothing) -> (blank, blank)

    return (doorTopImg, doorBottomImg)

-- ALUT
playSound :: SoundType -> RWSIO ()
playSound s = do
    env <- ask
    let soundContext = view (eSounds . sContext) env
        soundSources = view (eSounds . sSources) env
    withProgNameAndArgs runALUTUsingCurrentContext $ \ _ _ -> do
        currentContext $= Just soundContext
        Sound.play . maybeToList $ lookup s soundSources
-- ENDALUT

getCollidables :: [CellType] -- this is a list of collidables cell types
getCollidables = "*^" -- open to suggestions to improve this function :)

getLifeUpCellType :: [CellType]
getLifeUpCellType = "h"

getCoinCellType :: [CellType]
getCoinCellType = "c"

getKeyCellType :: [CellType]
getKeyCellType = "k"

getDoorCellType :: [CellType]
getDoorCellType = "tb"

getEnemyCellType :: [CellType]
getEnemyCellType = "X"

getSpikesCellType :: [CellType]
getSpikesCellType = "NSWE"

type Bounciness = Float
type BounceStop = Float
getBounciness :: CellType -> (Bounciness, BounceStop)
getBounciness c = case c of
    '^' -> (0.50, 0.05)
    '*' -> (0.25, 0.05)
    _   -> (0.00, 0.00)
