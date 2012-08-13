import NovelParser (run,novelParse)
import NovelInterface
import Graphics.UI.WX
import Control.Concurrent (threadDelay)
import TimerProcess
import IOProcess
import Menubar

main = start novelGame

--GUIのコントロール、及びイベント時のアクションを記述
novelGame :: IO()
novelGame = do
    menubar <- myMenuBar
    orders <- (run novelParse =<< readFile "text.txt")
    contents <- varCreate ([Save],Mes "" "",Nothing,Nothing,orders)
    f <- frame [text := "ノベルゲームエンジン"]
    p <- panel f [on paint := paintScreen contents]
    t <- timer f [interval := 20 , on command := updateScreen contents p]

    set p [on click := (\pt -> print "clicked")      -- 進行
          ,on (charKey 'm') := return ()
          ,on clickRight := showMenu p contents  -- menu表示
          ]
    set f menubar
    set f [layout := minsize (sz 800 600) $ widget p] where

--スクリーンの描画
paintScreen :: Var GameState -> DC a -> Rect -> IO()
paintScreen vContents dc viewArea = do
    (imgs,mes,music,branch,orders) <- varGet vContents
    set dc [brushColor := red , brushKind := BrushSolid]
    mapM_ (drawPart dc) imgs --描画対象それぞれを描画
    drawMes mes
    newOrders <- processOrder orders      --IOイベントの命令を処理
    playSound music
    drawBranch branch

--オブジェクトごとの描画を記述
drawPart dc order = case order of
    _ -> circle dc (Point 400 300) radius [] where radius = 10

--timerに設定した時間毎に画面及び命令リスト情報を更新する
updateScreen :: Var GameState -> Panel() -> IO()
updateScreen state p = do
    varUpdate state nextState
    repaint p

--メニュー画面の描画を記述(ファイルIOを含む)
showMenu p contents pt = do
    varUpdate contents id
    repaint p

menuScreen = Mes "" ""
