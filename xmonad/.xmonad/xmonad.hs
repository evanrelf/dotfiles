import XMonad
import XMonad.Core
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.EZConfig (additionalKeysP)

import Data.Function ((&))

myKeys =
  [ ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "10"])
  , ("<XF86MonBrightnessUp>"  , safeSpawn "light" ["-A", "10"])
  , ("<XF86AudioLowerVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>"        , safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  , ("M-p"                    , safeSpawn "rofi" ["-show", "run"])
  ]

myLayoutHook = layoutHook def
  & smartBorders
  & smartSpacingWithEdge 4

myConfig = def
  { terminal = "xst || st"
  , focusFollowsMouse = False
  , borderWidth = 2
  , normalBorderColor = "#383c4a"
  , focusedBorderColor = "#5294e2"
  , layoutHook = myLayoutHook
  , modMask = mod4Mask
  } `additionalKeysP` myKeys

main = do
  xmonad myConfig
