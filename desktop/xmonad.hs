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

main = do
  xmproc <- spawnPipe "/home/bryan/.cabal/bin/xmobar /home/bryan/.xmonad/xmobarrc"
  xmonad $ myConfig xmproc `additionalKeys` myAdditionalKeys

myConfig xmproc =
  defaultConfig
  { manageHook         = myManageHook
  , layoutHook         = myLayoutHook
  , logHook            = myLogHook xmproc
  , modMask            = myModMask
  , terminal           = myTerminal
  , normalBorderColor  = "#444444"
  , focusedBorderColor = "#FF0000"
  , borderWidth        = 1
  , workspaces         = myWorkspaces
  }

myModMask = mod4Mask

myTerminal = "urxvt"

myManageHook = manageDocks
               <+> manageSpawn
               <+> composeAll myRules
               <+> manageHook defaultConfig
  where
    myRules = [ className =? "Firefox"     --> doShift "2:web"
              , className =? "Chromium"    --> doShift "2:web"
              , className =? "Thunderbird" --> doShift "2:web"
              , className =? "Emacs"       --> doShift "1:emacs"
              ]

myWorkspaces = zipWith (++) numbers (labels ++ blanks)
  where
    numbers = map show [1.. 9]
    blanks  = "" : blanks
    labels  = [":emacs", ":web", ":terms"]

myLayoutHook = avoidStruts
               $ onWorkspace "2:web" fullscreen
               $ onWorkspace "4:terms" terms
               $ standards
  where
    standards  = tiled ||| Mirror tiled ||| Full
    fullscreen = Full ||| tiled ||| Mirror tiled
    terms      = Grid ||| tiled ||| Full
    tiled      = renamed [Replace "Default"] $ smartSpacing 20 $ Tall nmaster delta ratio
    nmaster    = 1
    ratio      = 1/2
    delta      = 3/100

myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppTitle  = xmobarColor "green" "" . shorten 50
                     }

myAdditionalKeys =
  [ ((mod, xK_Tab),         toggleWS)
  , ((mod, xK_Right),       nextWS)
  , ((mod, xK_Left),        prevWS)
  , ((modShift, xK_Right),  shiftToNext)
  , ((modShift, xK_Left),   shiftToPrev)
  ]
  where
    mod      = myModMask
    modShift = myModMask .|. shiftMask
