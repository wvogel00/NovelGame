import NovelParser
import NovelInterface
import Graphics.UI.WX
import Control.Concurrent (threadDelay)
import Text.Parsec (parseTest)

main = start novelGame

novelGame :: IO()
novelGame = do
    str <- readFile "text.txt"
    title <- parseTest nString str
    f <- frame [text := "ノベルゲームエンジン"]
    p <- panel f []
    set p [layout := column 1 [minsize (sz 100 100) $ label str] ]
    set f [layout := widget p , clientSize := sz 800 600]
