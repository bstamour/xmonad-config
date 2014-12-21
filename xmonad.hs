{-# LANGUAGE TemplateHaskell #-}


import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)

import System.IO
import Language.Haskell.TH
import Network.HostName


[d| myHostName = $(stringE =<< runIO getHostName ) |]


onLaptop :: Bool
onLaptop = myHostName == "cantor"


onDesktop :: Bool
onDesktop = myHostName == "planck"


main = do
  xmproc <- spawnPipe "/home/bryan/.cabal/bin/xmobar /home/bryan/.xmonad/xmobarrc"
  xmonad $ myConfig xmproc `additionalKeys` myAdditionalKeys


myConfig xmproc =
  defaultConfig
  { manageHook         = myManageHook
  , layoutHook         = myLayoutHook
  , workspaces         = myWorkspaces
  , logHook            = myLogHook xmproc
  , modMask            = myModMask
  , terminal           = myTerminal
  , normalBorderColor  = "#444444"
  , focusedBorderColor = "#FF0000"
  , borderWidth        = 1
  }


myModMask = mod4Mask


myTerminal = "urxvt"


myManageHook = manageDocks
               <+> manageSpawn
               <+> composeAll myRules
               <+> manageHook defaultConfig
  where
    myRules = [ className =? "Firefox"  --> doShift "1:web"
              , className =? "Emacs"    --> doShift "2:emacs"
              ]


myWorkspaces = zipWith (++) numbers (labels ++ blanks)
  where
    numbers   = map show [1.. 9]
    blanks    = "" : blanks
    labels    = [":web", ":emacs", ":terms"]


myLayoutHook = onWorkspace "3:terms" termsLayout $ standardLayout
  where
    termsLayout      = avoidStruts $ (Grid ||| tiled ||| Mirror tiled ||| Full)
    standardLayout   = avoidStruts $ (tiled ||| Mirror tiled ||| Full)

    tiled      = renamed [Replace "Default"] $ smartSpacing spacing $ Tall nmaster delta ratio
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100
    spacing    = if onLaptop then 0 else 10


myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppTitle  = xmobarColor "green" "" . shorten 50
                     }


-- The little launch key next to the mic key at the top.
myLaunchKey = 0x1008FF41


-- Volume control keys.
myRaiseVolumeKey = 0x1008ff13
myLowerVolumeKey = 0x1008ff11
myMuteVolumeKey  = 0x1008ff12


myAdditionalKeys =
  [ ((modShift, xK_p),      spawn "sudo /usr/sbin/pm-suspend")
  , ((0, myMuteVolumeKey),  spawn "amixer set Master toggle")
  , ((0, myRaiseVolumeKey), spawn "amixer set Master,0 2%+")
  , ((0, myLowerVolumeKey), spawn "amixer set Master,0 2%-")
  , ((mod, xK_Tab),         toggleWS)
  , ((mod, xK_Right),       nextWS)
  , ((mod, xK_Left),        prevWS)
  , ((modShift, xK_Right),  shiftToNext)
  , ((modShift, xK_Left),   shiftToPrev)
  ]
  where
    mod      = myModMask
    modShift = myModMask .|. shiftMask
