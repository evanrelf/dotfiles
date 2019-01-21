import Data.Function ((&))
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Layout.Maximize (maximizeRestore, maximizeWithPadding)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap)
import XMonad.Util.Run (safeSpawn)

main :: IO ()
main = xmobar myConfig >>= xmonad

myConfig = def
  { terminal = "st"
  , focusFollowsMouse = True
  , clickJustFocuses = True
  , borderWidth = 1
  , normalBorderColor = "#333333"
  , focusedBorderColor = "#999999"
  , startupHook = myStartupHook
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , handleEventHook = myHandleEventHook
  , logHook = myLogHook
  , modMask = myModMask
  }
  & (flip additionalKeysP) myKeys
  & (flip additionalMouseBindings) myMouse
  & docks
  & ewmh

myModMask = mod4Mask

myKeys =
  -- Window manager
  [ ("M-p", safeSpawn "rofi" ["-show", "run"])
  , ("M--", sendMessage MirrorShrink)
  , ("M-=", sendMessage MirrorExpand)
  , ("M-f", withFocused (sendMessage . maximizeRestore))
  , ("M-<Tab>", toggleWS)
  -- Brightness
  , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "5"])
  , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "5"])
  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]

myMouse =
  [ ((myModMask, button3), (\w -> focus w >> mouseResizeEdgeWindow (1/2) w))
  ]

myStartupHook = do
  return () -- Do not remove
  checkKeymap myConfig myKeys
  adjustEventInput

myLayoutHook =
  maximizeWithPadding 20 (ResizableTall 1 (1/20) (1/2) [])
  & spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  & smartBorders
  & avoidStruts

myManageHook = manageHook def
  <+> manageDocks

myHandleEventHook = handleEventHook def
  <+> fullscreenEventHook
  <+> focusOnMouseMove

myLogHook = updatePointer (0.5, 0.5) (0, 0)

{-# ANN module "HLint: Redundant return" #-}
