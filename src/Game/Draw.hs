module Game.Draw where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.RWS

import Game.Action
import Game.AssetManagement
import Game.Data.Alias
import Game.Data.Asset
import Game.Data.Enum
import Game.Data.Environment
import Game.Data.State
import Game.Input
import Game.Logic

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Data.Char

renderGame :: RWSIO Picture
renderGame = do
    env <- ask

    -- level cell/tiles pictures
    level        <- use (gLevelState . lLevelCells)
    layerBack    <- drawTiles "*tb"
    layerFront   <- drawTiles "^kch"
    
    -- player picture
    (x, y)       <- use (gPlayerState . pPosition)
    playerSprite <- getPlayerSprite
    let playerPos = (x, y - 4) -- offset
        playerPic = [uncurry translate playerPos playerSprite]
    
    -- bg & text pictures
    background   <- renderBackground
    creditsPic   <- scrollCredits
    playerHUD    <- renderHUD
    text         <- renderText
    
    -- title pictures
    titlePic     <- scaleTitle
    levelName    <- use (gLevelState . lLevelName)
    transition   <- use gTransition
    let tX            = 256 * min 0 transition
        posTitle      = ( tX,  32)
        posSubtitle   = (-tX, -64)
        lvlTitles = view (eAssets . aLvlTitles   ) env
        lvlSubs   = view (eAssets . aLvlSubtitles) env
        (w, h)    = (fromIntegral $ view eWindowWidth env,
                     fromIntegral $ view eWindowHeight env)
        lvlTitle  = if tX < -w then []
                    else case lookup levelName lvlTitles of
                        Just title -> [uncurry translate posTitle title]
                        Nothing    -> []
        lvlSub    = if tX < -h then []
                    else case lookup levelName lvlSubs of
                        Just sub   -> [uncurry translate posSubtitle sub]
                        Nothing    -> []
    
    -- frame: to hide out-of-window objects, to avoid breaking the parallax
    screenSize <- liftIO getScreenSize
    let sW = fromIntegral . fst $ screenSize
        sH = fromIntegral . snd $ screenSize
        frame =
            [ translate   0   h  . color black $ rectangleSolid sW  h
            , translate (-w)  0  . color black $ rectangleSolid  w sH
            , translate   w   0  . color black $ rectangleSolid  w sH
            , translate   0 (-h) . color black $ rectangleSolid sW  h
            ]
    
    scene <- use gGameScene
    return . pictures $ case scene of 
        ScenePause     ->
            background ++
            layerBack  ++
            playerPic  ++ 
            layerFront ++
            playerHUD  ++
            text       ++
            frame
        SceneStart     ->
            background ++
            playerPic  ++ 
            titlePic   ++
            text       ++
            frame
        SceneCredits   ->
            background ++ 
            creditsPic {- ++
            frame -}
        SceneLevel     ->
            if transition < 0
                then -- put level name title and subtitle at the back
                    background ++
                    lvlTitle   ++
                    lvlSub     ++
                    layerBack  ++
                    playerPic  ++
                    layerFront ++
                    playerHUD  ++
                    text       ++
                    frame
                else -- put level name title and subtitle at the front
                    background ++
                    layerBack  ++
                    playerPic  ++
                    layerFront ++
                    lvlTitle   ++
                    lvlSub     ++
                    playerHUD  ++
                    text       ++
                    frame
        SceneWin       ->
            background ++
            frame
        SceneLose      ->
            background ++
            layerBack  ++
            playerPic  ++ 
            layerFront ++
            playerHUD  ++
            text       ++
            frame
        
    

updateGame :: Float -> RWSIO GameState
updateGame sec = do
    env       <- ask
    gDeltaSec .= sec
    gSec    %= (+sec)
    
    scene <- use gGameScene
    case scene of
        ScenePause -> 
            return () -- update nothing
        SceneLevel -> do
            movePlayer
            incPlayerSprite
            -- ALUT
            -- playSFX
            -- ENDALUT
            
            incKeys
            incLives
            
            updatedLevel  <- removeItem
            gLevelState . lLevelCells .= updatedLevel
            
            door <- openDoor
            gDoorOpen .= door
            
            gTimeRemaining %= (+negate sec)
            
            timeUp
            checkDoor
            updateParallax
            updateTransition
        SceneStart -> do
            sec      <- use gSec
            (pos, _) <- use (gPlayerState . pPosition)
            let width = fromIntegral $ view eWindowWidth env
                (x,y) = (width * sin (sec / 2 - pi / 2),-200)
            gPlayerState . pPosition .= (x, y)
            if x < pos
                then do
                    gPlayerState . pHeading  .= FaceLeft
                    gPlayerState . pMovement .= MoveLeft
                else do
                    gPlayerState . pHeading  .= FaceRight
                    gPlayerState . pMovement .= MoveRight
            incPlayerSprite
        SceneCredits -> do
            sec <- use gSec -- reset game after 1-minute idle
            when (sec > 60) resetGame
        _ ->
            return ()
    get --  return GameState

-- Helper Functions:
renderTile :: (PureRWS m) => CellType -> m Picture
renderTile cellType = do
    env <- ask
    let baseImg  = view (eAssets . aBase ) env
        grassImg = view (eAssets . aGrass) env
        coinImg  = head $ view (eAssets . aCoin) env
        keyImg   = view (eAssets . aKey  ) env
        doorImgs = view (eAssets . aDoor ) env
        heartImg = view (eAssets . aHeart) env
    
    isDoorOpen <- use gDoorOpen
    doorTup    <- getDoorSprite
    
    return $ case cellType of
        '*' -> baseImg 
        '^' -> grassImg
        'c' -> coinImg
        'k' -> fst keyImg
        't' -> fst doorTup
        'b' -> snd doorTup
        'h' -> heartImg
        _   -> circle 0 -- should never reach here
    

drawTiles :: (PureRWS m) => [CellType] -> m [Picture]
drawTiles cellTypes = do
    level <- use (gLevelState . lLevelCells)
    let  tiles = filter ((`elem` cellTypes) . snd) level
    forM tiles (\ (pos, cell) -> do
        tile  <- renderTile cell
        return . uncurry translate pos $ tile)
    

renderText :: (PureRWS m) => m [Picture]
renderText = do
    env          <- ask
    scene        <- use gGameScene
    level        <- use (gLevelState . lLevelName)
    sec          <- use gSec
    
    let blink     = even . truncate $ sec * 2
        continue  = if blink
            then [view (eAssets . aTxtPause) env]
            else []
        title     = view (eAssets . aTxtTitle) env
        -- width     = fromIntegral $ view eWindowWidth  env
        height    = fromIntegral $ view eWindowHeight env
        -- gameover  = [scale 0.75 0.75 $ view (eAssets . aTxtGameover) env]
        gameover  = if blink
            then [ translate 0 ( height / 6) $ view (eAssets . aTxtGameover) env]
            else [ translate 0 ( height / 6) $ view (eAssets . aTxtGameover) env
                 , translate 0 (-height / 6) $ view (eAssets . aTxtContinue) env
                 ]
        enter     = view (eAssets . aTxtEnter) env
        startText = if blink && sec * 5 > 15
            then [translate 0 (-150) enter]
            else [] 
    return $
        case scene of
            ScenePause -> continue
            SceneStart -> startText
            SceneLose  -> gameover
            _          -> []

--Will fix up numbers 
scaleTitle :: (PureRWS m) => m [Picture]
scaleTitle = do
    env <- ask
    sec <- use gSec
    let rate    = 5 -- Rate in which the picture scales based on ticks
        tick    = sec * rate -- (120 - timeRemaining) * rate 
        title   = view (eAssets . aTxtTitle) env
        newTick = min 15 tick
        pulse   = 1 + sin (max 15 tick - 15) / 15
        scaleXY = pulse * newTick / 10
        pic     = scale scaleXY scaleXY $ uncurry translate (0,50) title
    return [pic]

--Might need to retake the credits image as the first line is off center..
scrollCredits :: (PureRWS m) => m [Picture]
scrollCredits = do
    env <- ask
    sec <- use gSec
    -- timeRemaining <- use gTimeRemaining
    let credits = view (eAssets . aTxtCredits ) env
        skipImg = view (eAssets . aTxtContinue) env
        blink   = even . truncate $ sec * 2
        rate    = 30 -- Rate in which the picture scrolls based on ticks
        tick    = sec * rate -- Uses seconds gone by to reduce to a value below 2 
        height  = fromIntegral $ view eWindowHeight env
        pic     = scale 0.75 0.75 $ translate   0 (tick - height) credits
        skip    = scale 0.50 0.50 $ translate 555 (-700)          skipImg
    return $ if sec > 25 && blink
        then [pic, skip]
        else [pic]
    

renderBackground :: (PureRWS m) => m [Picture]
renderBackground = do
    env <- ask
    level <- use (gLevelState . lLevelName)
    scene <- use (gGameScene) 

    let lvlList = (view (eAssets . aLvlNames) env)
        bgImgs = view (eAssets . aBgImg ) env
        zipLvls = zip lvlList bgImgs
        imgToUse = 
            case scene of 
                SceneStart    -> lookup (show LevelStart) zipLvls
                SceneCredits  -> lookup (show LevelCredits) zipLvls
                _             -> lookup (show level) zipLvls

    parallax <- use gParallax
    case imgToUse of
        Just bg -> return [uncurry translate parallax bg]
        Nothing -> return []

updateParallax :: (PureRWS m) => m ()
updateParallax = do
    d      <- use gDeltaSec
    (x, y) <- use (gPlayerState . pPosition)
    
    let smooth a b = a + 5 * d * signum c * abs c where c = b - a
        moveTo (x1, y1) (x2, y2) = (smooth x1 x2, smooth y1 y2)
        target = (-x/5, -y/25)
    gParallax %= (`moveTo` target)

updateTransition :: (PureRWS m) => m ()
updateTransition = do
    sec <- use gDeltaSec
    gTransition %= (+negate sec)

renderHUD :: (PureRWS m) => m [Picture]
renderHUD = do
    hearts <- drawHearts
    timer  <- drawTimer
    return $ hearts ++ timer

drawHearts :: (PureRWS m) => m [Picture]
drawHearts = do
    env    <- ask
    (x, y) <- use (gPlayerState . pPosition)
    livesCount <- fromIntegral <$> use (gPlayerState . pLives)
    let startX  = x - 5 * (livesCount - 1)
        startY  = view eTileSize env
        heart   = view (eAssets . aHeartSmall) env
        hearts  = map  (\ l ->
            let hX = startX + l * 10
                hY = startY + y
            in  translate hX hY heart) [0 .. livesCount - 1]
    return hearts

-- renderDigits :: String -> [Picture] -> [Picture]
-- renderDigits [] _ = []
-- renderDigits (x:xs) digits 
--             | x == '-'  = [digits !! 0]                               -- keep showing 0 when timer goes negative
--             | otherwise = digits !! read [x] : renderDigits xs digits

-- addShift :: [Picture] -> Float -> Float -> [Picture]
-- addShift [] _ _ = []  -- 30 is width of digit picture
-- addShift (x:xs) xPos yPos = (translate (xPos - fromIntegral (30 * length xs)) yPos x) : (addShift xs xPos yPos)

drawTimer :: (PureRWS m) => m [Picture]
drawTimer = do
    env <- ask
    (x, y)          <- use (gPlayerState . pPosition)
    timeRemaining   <- max 0 <$> use gTimeRemaining
    let timerText    = show $ ceiling timeRemaining
        -- windowWidth  = view eWindowWidth  env
        -- windowHeight = view eWindowHeight env
        tileSize     = view eTileSize env
        fontSize     = 16
        halfSize     = 8
        digits       = view (eAssets . aTxtDigits) env
        timerPics    = map ((!!) digits . (subtract $ ord '0') . ord) timerText
        -- timerPics    = renderDigits timerText digits
        -- xPos         = fromIntegral windowWidth  / 2 - tileSize / 2
        -- yPos         = fromIntegral windowHeight / 2 - tileSize / 2
        startX       = x - halfSize * fromIntegral (length timerText - 1)
        startY       = y + fontSize + tileSize
        timer        = map (\ (p, i) ->
            let tX = startX + i * fontSize
                tY = startY
            in  translate tX tY $ scale 0.5 0.5 p) $
            zip timerPics [0..]
    return timer

-- ALUT
-- playSFX :: RWSIO ()
-- playSFX = do
--     player <- use (gPlayerState . pPosition)
--     let coin = getCoinCellType
--         key  = getKeyCellType
--         door = getDoorCellType
    
--     hitCoin <- collideWith coin player
--     case hitCoin of
--         Just cn -> playSound Coin
--         Nothing -> return ()
    
--     hitKey <- collideWith key player
--     case hitKey of
--         Just ky -> playSound Key
--         Nothing -> return ()
    
--     hitDoor <- collideWith door player
--     isDoorOpen <- use gDoorOpen
--     when isDoorOpen $ case hitDoor of
--         Just dr -> playSound DoorClose
--         Nothing -> return ()
-- ENDALUT