module Menubar where

import Graphics.UI.WX
import Graphics.UI.WX.Menu

myMenuBar = do
 file <- menuPane [text := "&File"]
 mClose <- menuItem file [text := "$Close",
                          help := "close window"]

 bHelp <- menuPane [text:="&Help"]
 mVersion <- menuItem bHelp [text := "Version",
                             help := "version info."]
 mHelp    <- menuItem bHelp [text := "Help",
                             help := "hot to use application"]

 return [ menuBar := [file,bHelp]
         ,on (menu mClose):= return ()
         ,on (menu mHelp) := return ()
         ,on (menu mVersion) := return ()]

