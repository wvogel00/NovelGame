module NovelInterface where

data Order =  Clear
            | ClickWait
            | Wait Int
            | Mes String String
            | Music String Loop
            | MusicStop
            | Image Int (Int,Int) FilePath
            | Branch [(String,String)]
            | Save
            | Load
            | Menu [SaveData] [Option]  deriving (Eq,Show)

type Loop = Bool

data Option = Option {
    soundOn :: Bool,
    readLog :: String
} deriving (Eq,Show)

data SaveData = SaveData {
    date :: Date,
    file :: String,
    line :: Int,
    images :: [Order]
} --deriving (Show,Eq)

instance Eq SaveData where
  d1 == d2 =  True

instance Show SaveData where
    show sd = show y ++ concat (map ((":"++).show) [m,d,h,min,s] ) where
        y = year $ date sd
        m = month $ date sd
        d = day $ date sd
        h = hour $ date sd
        min = minute $ date sd
        s = second $ date sd

data Date = Date {
    year   :: Int,
    month  :: Int,
    day    :: Int,
    hour   :: Int,
    minute :: Int,
    second :: Int
    } deriving Show
