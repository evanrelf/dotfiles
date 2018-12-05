import XMonad
import XMonad.Core
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)

import Data.Function ((&))

myKeys =
  [ ("<XF86MonBrightnessDown>", safeSpawn "xbacklight" ["-10", "-time", "50"])
  , ("<XF86MonBrightnessUp>"  , safeSpawn "xbacklight" ["+10", "-time", "50"])
  , ("<XF86AudioLowerVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "2%-"])
  , ("<XF86AudioRaiseVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "2%+"])
  , ("<XF86AudioMute>"        , safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  , ("M-p"                    , safeSpawn "rofi" ["-show", "run"])
  ]

myLayoutHook = layoutHook def
  & smartBorders
  & smartSpacingWithEdge 4

myConfig = def
  { terminal = "st"
  , focusFollowsMouse = False
  , borderWidth = 2
  , normalBorderColor = "#383c4a"
  , focusedBorderColor = "#5294e2"
  , layoutHook = myLayoutHook
  , modMask = mod4Mask
  } `additionalKeysP` myKeys

main = do
  xmobar <- spawnPipe "xmobar"
  xmonad myConfig
