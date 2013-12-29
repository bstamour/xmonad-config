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
  , focusedBorderColor = "#005577"
  , borderWidth        = 2
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


myLayoutHook = standards
  where
    standards = tiled ||| Mirror tiled ||| paddedFull ||| full ||| grid

    tiled       = avoidStruts $ Tall nmaster delta ratio
    paddedFull  = avoidStruts $ padding 0 200  Full
    full        = avoidStruts Full
    grid        = avoidStruts Grid

    nmaster = 1
    ratio   = 1/2
    delta   = 3/100


myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppTitle  = xmobarColor "green" "" . shorten 50
                     }


-- The little launch key next to the mic key at the top.
myLaunchKey = 0x1008FF41


myAdditionalKeys =
  [ ((myModMask .|. shiftMask, xK_p), spawn "sudo /usr/sbin/pm-suspend")
  , ((0, myLaunchKey),                spawn "sudo /sbin/shutdown -h now")
  , ((myModMask, xK_m),               spawn (myTerminal ++ " -e mutt"))
  ]
