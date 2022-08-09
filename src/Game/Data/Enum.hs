module Game.Data.Enum where

-- This is to control render and update functions
data GameScene
    = SceneMenu
    | SceneLevel
    | SceneTransition
    | SceneWin
    | SceneLose
    | ScenePause

-- This is for player movement logic
data PlayerMovement
    = MoveStop
    | MoveLeft
    | MoveRight

-- This is for player sprite drawing
data PlayerFacing
    = FaceLeft
    | FaceRight
