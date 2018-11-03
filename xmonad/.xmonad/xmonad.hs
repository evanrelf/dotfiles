import XMonad
import XMonad.Core
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
  , ("M-p"                     , spawn "rofi -show run")
  ]

myLayoutHook = layoutHook def
  & smartBorders
  & smartSpacingWithEdge 4
  & avoidStrutsOn [U, D]

myConfig = defaultConfig
  { terminal = "st"
  , focusFollowsMouse = False
  , borderWidth = 1
  , normalBorderColor = "#383c4a"
  , focusedBorderColor = "#5294e2"
  , layoutHook = myLayoutHook
  , modMask = mod4Mask
  } `additionalKeysP` myKeys

main :: IO ()
main = xmonad myConfig
