module Process where

import NovelInterface

nextState :: GameState -> GameState
mextState (_,_,_,_,[]) = ([],Mes "" "",Nothing,[])
nextState state@(imgs,mes,music,branch,(o:orders)) = case o of
    Clear -> (imgs,Mes "" "" , music,branch,orders)
    NewLine -> updateNewLine state
    Mes k str -> updateMes k str state
    Music file t -> updateMusic (Just $ Music file t) state
    MusicStop -> updateStopMusic state
    Branch br -> updateBranch (Just $ Branch br) state
    Image l pos file -> updateImg (Image l pos file) state
    _ -> state

updateNewLine (is,Mes k str,sd,br,os) = (is,Mes k (str++"\n"),sd,br,os)
updateMusic sd (is,mes,_,br,os) = (is,mes,sd,br,os)
updateStopMusic (is,mes,_,br,os) = (is,mes,Nothing,br,os)
updateBranch br (is,mes,sd,_,os) = (is,mes,sd,br,os)

--画像情報を更新
updateImg img (is,mes,sd,br,os) = (sortImg img is, mes, sd, br, tail os)
--レイヤー順に画像を並べる
sortImg img [] = [img]
sortImg img@(Image l1 _ _ ) imgs@(Image l2 p file :is)
    | l1 < l2   = img:imgs
    | l1 == l2  = img:is
    | otherwise = Image l2 p file : sortImg img is

--描画文字列を更新
updateMes k str (is,mes,sd,br,os)
    = case nextMes of
        Just ms -> (is,newMes,sd,br,ms:tail os)
        Nothing -> (is,newMes,sd,br,tail os)
        where (newMes,nextMes) = updateMes' mes k str

updateMes' mes k2 "" = (mes,Nothing)
updateMes' (Mes k1 s1) k2 (s:s2) = if k1 == k2
    then (Mes k1 (s1++[s]) , Just $ Mes k1 s2)
    else (Mes k2       [s] , Just $ Mes k2 s2)
