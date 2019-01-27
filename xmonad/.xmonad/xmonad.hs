import Data.Function ((&))
import XMonad
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), moveTo, shiftTo, toggleWS')
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.StackSet (RationalRect(..))
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)

-- import XMonad.Hooks.InsertPosition (Focus(..), Position(..), insertPosition)
import XMonad.Util.Loggers as Log

-- TODO:
-- * Disable mouse snapping when resizing window with mouse
-- * Disable/exit Full layout when only one window

main :: IO ()
main = xmobar myConfig >>= xmonad

xmobar = DL.statusBar "xmobar" pp toggleStrutsKey
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
    pp =
      let battery = "echo -n $(acpi | cut -d ' ' -f 4-5 | sed 's/, 0\\?/ (/g' | cut -d ':' -f 1-2)')'"
          dateTime = "date '+%a %b %d %I:%M %p' | sed 's/ 0/ /g'"
      in DL.xmobarPP
          { DL.ppHidden = (\w -> if w == "NSP" then "" else w)
          , DL.ppCurrent = DL.xmobarColor "white" "" . DL.wrap "[" "]"
          , DL.ppTitle = const "" -- DL.xmobarColor "white" "" . DL.shorten 40
          -- , DL.ppExtras = Log.logCmd <$> [battery, dateTime]
          }

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
  [ ("M-/", safeSpawn "rofi" ["-show", "run"])
  , ("M--", sendMessage MirrorShrink)
  , ("M-=", sendMessage MirrorExpand)
  , ("M-f", sendMessage ToggleLayout)
  , ("M-<Tab>", toggleWS' ["NSP"])
  , ("M-n", moveTo Next NonEmptyWS)
  , ("M-p", moveTo Prev NonEmptyWS)
  , ("M-S-n", shiftTo Next NonEmptyWS)
  , ("M-S-p", shiftTo Prev NonEmptyWS)
  , ("M-s", scratchpadSpawnActionCustom "st -n scratchpad")
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
  let tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (1/2) []
  in toggleLayouts Full tall
  & spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  & smartBorders
  & avoidStruts
  & renamed [CutWordsLeft 1]

myManageHook = manageHook def
  <+> manageDocks
  <+> scratchpadManageHook (RationalRect (1/4) (1/4) (1/2) (1/2))

myHandleEventHook = handleEventHook def
  <+> fullscreenEventHook
  <+> focusOnMouseMove

myLogHook = logHook def
  <+> updatePointer (0.5, 0.5) (0, 0)

{-# ANN module "HLint: Redundant return" #-}
