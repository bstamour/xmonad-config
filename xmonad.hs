import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspace)
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


myModMask = mod1Mask


myTerminal = "urxvt"


myManageHook = manageDocks        <+>
               manageSpawn        <+>
               composeAll myRules <+>
               manageHook defaultConfig
  where
    myRules = [ title     =? "mutt"    --> doShift "1:mail"
              , className =? "Firefox" --> doShift "2:web"
              , className =? "Emacs"   --> doShift "3:code"
              ]


myWorkspaces = ["1:mail", "2:web", "3:code", "4:terms", "5", "6" ,"7", "8", "9","10"]


myLayoutHook = avoidStruts                 $
               onWorkspace "2:web" web     $
               onWorkspace "4:terms" terms $
               standards
  where
    -- Per-workspace layout schemes.
    standards = tiled ||| Mirror tiled ||| Full
    web       = paddedFull ||| Full ||| tiled ||| Mirror tiled
    terms     = Grid

    -- Layouts.
    paddedFull  = padding 0 200  Full
    tiled       = Tall nmaster delta ratio
      where
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100


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
  [ ((myModMask .|. shiftMask, xK_p), spawn "sudo /usr/sbin/pm-suspend")
  , ((0, myLaunchKey),                spawn "sudo /sbin/shutdown -h now")
  , ((0, myMuteVolumeKey),            spawn "amixer set Master toggle")
  , ((0, myRaiseVolumeKey),           spawn "amixer set Master,0 2%+")
  , ((0, myLowerVolumeKey),           spawn "amixer set Master,0 2%-")
  , ((myModMask, xK_m),               spawn (myTerminal ++ " -e mutt"))
  ]
