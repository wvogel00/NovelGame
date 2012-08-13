module Process where

import NovelInterface

nextState :: GameState -> GameState
mextState (_,_,_,_,[]) = ([],Mes "" "",Nothing,[])
nextState state@(imgs,mes,music,branch,(o:orders)) = case o of
    Mes k str -> (imgs,newMs,music,branch,nextOrders nextMs orders) where
        (newMs,nextMs) = updateMes mes k str
    Clear -> (imgs,Mes "" "" , music,branch,orders)
    ClickWait -> state
    Save -> state
    Load -> state
    NewLine -> (imgs,append mes '\n',music,branch,orders) where
        append (Mes k str) c = Mes k $ str++[c]



nextOrders :: Maybe Order -> [Order] -> [Order]
nextOrders (Just a) os = a:os
nextOrders Nothing os = os

updateMes mes k2 "" = (mes,Nothing)
updateMes (Mes k1 s1) k2 s2
 = if k1 == k2 then (Mes k1 (s1++[head s2]) , Just $ Mes k1 $ tail s2 )
               else (Mes k2 [head s2] , Just $ Mes k2 $ tail s2)
