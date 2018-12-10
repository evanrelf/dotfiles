import Data.Function ((&))
import XMonad
import XMonad.Core
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (safeSpawn, spawnPipe)

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
  -- & smartSpacingWithEdge 2
  & avoidStruts

myManageHook = manageHook def <+> manageDocks

myHandleEventHook = handleEventHook def <+> fullscreenEventHook

myConfig = def
  { terminal = "xst"
  , focusFollowsMouse = True
  , borderWidth = 2
  , normalBorderColor = "#383c4a"
  , focusedBorderColor = "#5294e2"
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , handleEventHook = myHandleEventHook
  , modMask = mod4Mask
  } `additionalKeysP` myKeys

main = do
  polybar <- spawnPipe "pkill polybar; sleep 0.2; polybar top"
  xmonad (myConfig & docks & ewmh)
