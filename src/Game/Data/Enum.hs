module Game.Data.Enum where

-- This is to control render and update functions
data GameScene
    = SceneLevel
    | SceneWin
    | SceneLose
    | ScenePause
    | SceneCredits
    | SceneStart
    deriving Show

-- This is for player movement logic
data PlayerMovement
    = MoveStop
    | MoveLeft
    | MoveRight

-- This is for player sprite drawing
data PlayerFacing
    = FaceLeft
    | FaceRight

-- Determines current level
data LevelName
    = Level1
    | Level2
    | Level3
    | LevelCredits
    | LevelStart
    deriving (Show, Eq, Bounded, Enum)
