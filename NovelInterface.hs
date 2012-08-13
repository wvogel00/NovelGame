module NovelInterface where

--[Image]、Mes,Maybe Music,Maybe Branch,命令群の情報を持つ
type GameState = ([Order],Order,Maybe Order,Maybe Order,[Order])


data Order =  Clear
            | ClickWait
            | NewLine
            | Wait Int
            | Mes String String
            | Music String Loop
            | MusicStop
            | Image Int (Int,Int) FilePath
            | Branch [(String , String , Maybe String)]
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
    images :: [Order],
    bgMusic :: String
} deriving (Show,Eq)

data Date = Date {
    year   :: Int,
    month  :: Int,
    day    :: Int,
    hour   :: Int,
    minute :: Int,
    second :: Int
    }-- deriving Show

instance Eq Date where
    date1 == date2 = True

instance Show Date where
    show dt = show y ++ (concat $ map ((":"++).show) [m,d,h,min,s]) where
        y = year dt
        m = month dt
        d = day dt
        h = hour dt
        min = minute dt
        s = second dt
