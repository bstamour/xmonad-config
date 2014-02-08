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

import Padding
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
    myRules = [ title     =? "mutt"    --> doShift "1:mail"
              , className =? "Firefox" --> doShift "2:web"
              , className =? "Emacs"   --> doShift "3:code"
              ]

myWorkspaces = ["1:mail", "2:web", "3:code", "4:terms", "5", "6" ,"7", "8", "9","10"]

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

-- The little launch key next to the mic key at the top.
myLaunchKey = 0x1008FF41

-- Volume control keys.
myRaiseVolumeKey = 0x1008ff13
myLowerVolumeKey = 0x1008ff11
myMuteVolumeKey  = 0x1008ff12

myAdditionalKeys =
  [ ((modShift, xK_p),      spawn "sudo /usr/sbin/pm-suspend")  -- Suspend
  , ((0, myLaunchKey),      spawn "sudo /sbin/shutdown -h now") -- Shutdown
  , ((0, myMuteVolumeKey),  spawn "amixer set Master toggle")   -- Toggle sound mute
  , ((0, myRaiseVolumeKey), spawn "amixer set Master,0 2%+")    -- Raise volume
  , ((0, myLowerVolumeKey), spawn "amixer set Master,0 2%-")    -- Lower volume
  , ((mod, xK_m),           spawn (myTerminal ++ " -e mutt"))   -- Launch mail
  , ((mod, xK_Tab),         toggleWS)                           -- toggle last workspace (super-tab)
  , ((mod, xK_Right),       nextWS)                             -- go to next workspace
  , ((mod, xK_Left),        prevWS)                             -- go to prev workspace
  , ((modShift, xK_Right),  shiftToNext)                        -- move client to next workspace
  , ((modShift, xK_Left),   shiftToPrev)                        -- move client to prev workspace
  ]
  where
    mod      = myModMask
    modShift = myModMask .|. shiftMask
