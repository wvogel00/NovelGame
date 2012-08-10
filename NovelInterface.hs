module NovelInterface where

data Order =  Clear
            | ClickWait
            | Wait Int
            | Mes String String
            | Music String
            | MusicStop
            | Image (Int,String)
            | Branch [(String,String)]
            | Save
            | Load deriving (Eq,Show)
