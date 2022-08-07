module Game.Data.Enum where

data GameScene
    = SceneMenu
    | SceneLevel
    | SceneTransition
    | SceneWin
    | SceneLose
    | ScenePause

data PlayerFacing
    = FaceLeft
    | FaceRight
    deriving (Eq)
