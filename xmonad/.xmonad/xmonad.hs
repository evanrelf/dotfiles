import XMonad
import XMonad.Core
import XMonad.Config.Xfce (xfceConfig)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.EZConfig (additionalKeysP)

import Data.Function ((&))

myKeys =
  [ ("<XF86MonBrightnessDown>" , spawn "xbacklight -10 -time 50")
  , ("<XF86MonBrightnessUp>"   , spawn "xbacklight +10 -time 50")
  , ("<XF86AudioLowerVolume>"  , spawn "amixer -q sset Master 2%-")
  , ("<XF86AudioRaiseVolume>"  , spawn "amixer -q sset Master 2%+")
  , ("<XF86AudioMute>"         , spawn "amixer -q sset Master toggle")
  , ("M-p"                     , spawn "dmenu_run -i -fn 'Noto Sans' -nf '#d3dae3' -nb '#383c4a' -sf '#ffffff' -sb '#5294e2'")
  ]

myLayoutHook = layoutHook def
  & smartBorders
  & smartSpacingWithEdge 4
  & avoidStrutsOn [U, D]

myConfig = xfceConfig
  { terminal = "termite"
  , focusFollowsMouse = False
  , borderWidth = 1
  , normalBorderColor = "#383c4a"
  , focusedBorderColor = "#5294e2"
  , layoutHook = myLayoutHook
  } `additionalKeysP` myKeys

main :: IO ()
main = xmonad myConfig
