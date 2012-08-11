import NovelParser
import NovelInterface
import Graphics.UI.WX
import Control.Concurrent (threadDelay)
import Text.Parsec (parseTest)

main = start novelGame

--GUIのコントロール、及びイベント時のアクションを記述
novelGame :: IO()
novelGame = do
    contents <- varCreate []
    str <- readFile "text.txt"
    title <- parseTest nString str
    let orders = [Mes "" "はじまりはじまりー"]
    f <- frame [text := "ノベルゲームエンジン"]
    p <- panel f [on paint := paintScreen contents]
    set p [on click := (\pt -> print "clicked")      -- 進行
          ,on (charKey 'm') := playLoop $ sound "test.mp3" -- sound test
          ,on clickRight := showMenu p contents  -- menu表示
          ]
    set f [layout := minsize (sz 800 600) $ widget p] where

--スクリーンの描画
paintScreen :: Var [Order] -> DC a -> Rect -> IO()
paintScreen vContents dc viewArea = do
    contents <- varGet vContents
    set dc [brushColor := red , brushKind := BrushSolid]
    mapM_ (drawPart dc) contents --描画対象それぞれを描画

--オブジェクトごとの描画を記述
drawPart dc order = case order of
    _ -> circle dc (Point 400 300) radius [] where radius = 10

--メニュー画面の描画を記述(ファイルIOを含む)
showMenu p  contents pt = do
    varUpdate contents (menuScreen:)
    repaint p

menuScreen = Mes "" ""
