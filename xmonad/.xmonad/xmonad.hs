import Data.Function ((&))
import XMonad
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), moveTo, shiftTo, toggleWS)
import XMonad.Core
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Layout.MultiToggle (Toggle(..), mkToggle, single)
import XMonad.Layout.NoBorders (smartBorders)
-- import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (smartSpacingWithEdge)
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import XMonad.Util.Run (safeSpawn, spawnPipe)

-- TODO: Disable Full layout when there is only one window in a workspace
-- TODO: Add keys to swap workspaces

myKeymap =
  -- Brightness
  [ ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "5"])
  , ("<XF86MonBrightnessUp>"  , safeSpawn "light" ["-A", "5"])
  -- Volume
  , ("<XF86AudioLowerVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>" , safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>"        , safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  -- Rofi
  , ("M-/"                    , safeSpawn "rofi" ["-show", "drun"])
  , ("M-S-/"                  , safeSpawn "rofi" ["-show", "window"])
  -- Go to next/previous non-empty workspace
  , ("M-p"                    , moveTo Prev NonEmptyWS)
  , ("M-n"                    , moveTo Next NonEmptyWS)
  -- Move window to next/previous workspace
  , ("M-S-p"                  , shiftTo Prev NonEmptyWS)
  , ("M-S-n"                  , shiftTo Next NonEmptyWS)
  -- Go to last workspace
  , ("M-`"                    , toggleWS)
  -- Resize window
  -- , ("M--"                    , sendMessage MirrorShrink)
  -- , ("M-="                    , sendMessage MirrorExpand)
  ]

myStartupHook = return () >> checkKeymap myConfig myKeymap

myLayoutHook =
  {- ResizableTall 1 (1/10) (1/2) [] ||| -} Tall 1 (1/10) (1/2) ||| Full
    & smartBorders
    -- & smartSpacingWithEdge 7
    & avoidStruts
  -- safeSpawn "polybar-msg" ["-p", {- polybar pid -}, "top", "layout", "1"]

myManageHook = manageHook def <+> manageDocks

myHandleEventHook = handleEventHook def <+> fullscreenEventHook

myConfig = def
  { terminal = "st"
  , focusFollowsMouse = True
  , borderWidth = 2
  , normalBorderColor = "#383C4A"
  , focusedBorderColor = "#777777"
  , startupHook = myStartupHook
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , handleEventHook = myHandleEventHook
  , modMask = mod4Mask
  } `additionalKeysP` myKeymap

main = do
  polybar <- spawnPipe "polybar -r top"
  xmonad (myConfig & docks & ewmh)
