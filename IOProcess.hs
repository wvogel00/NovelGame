module IOProcess where

import Graphics.UI.WX
import Graphics.UI.WXCore (bitmapGetSize)
import NovelInterface
import Control.Concurrent (threadDelay)
import Control.Monad (foldM_)

drawImages :: DC a -> [Order] -> IO()
drawImages dc = foldM_ drawImg dc

drawImg :: DC a -> Order -> IO (DC a)
drawImg dc (Image _ pos file) = do
    bm <- bitmapCreateFromFile file
    --size <- bitmapGetSize bm
    drawBitmap dc bm (uncurry point pos) False []
    return dc

drawMes :: Order -> IO()
drawMes (Mes k str) = return () --print k >> print str

processOrder :: [Order] -> IO [Order]
processOrder order@(o:os) = case o of
    Wait time -> threadDelay (time*10^6) >> return os
    ClickWait -> return order
    _         -> return order

playSound :: Maybe Order -> IO()
playSound Nothing = return ()
playSound (Just music) = case music of
    Music file True -> return () --playLoop $ sound file
    Music file False -> return () --play $ sound file

drawBranch :: Maybe Order -> IO()
drawBranch Nothing = return ()
drawBranch (Just (Branch br)) = foldM_ drawBranch' (10,10) br
    where drawBranch' pos (str,label,file) = return pos
