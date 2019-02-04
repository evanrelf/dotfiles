import Data.Function ((&))
import System.Exit (ExitCode(..), exitWith)
import XMonad
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), moveTo, shiftTo, toggleWS')
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition (Focus(..), Position(..), insertPosition)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import qualified XMonad.Layout.Decoration as Deco
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.Tabbed (shrinkText, tabbed)
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet (RationalRect(..))
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap)
import XMonad.Util.Loggers (logCmd)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import qualified XMonad.Util.Themes as Themes

-- TODO:
-- * Disable mouse snapping when resizing window with mouse
-- * Format xmobar (brightness, volume, battery, wifi, date, time)

main :: IO ()
main = xmobar myConfig >>= xmonad

xmobar = DL.statusBar "xmobar" pp toggleStrutsKey
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
    pp = DL.xmobarPP
      { DL.ppHidden = (\w -> if w == "NSP" then "" else w)
      , DL.ppCurrent = DL.xmobarColor "white" "" . DL.wrap "[" "]"
      , DL.ppTitle = const ""
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

myFont = "xft:Terminus:style=Regular:size=12:antialias=false"

myKeys =
  -- Window manager
  [ ("M-/", safeSpawn "rofi" ["-show", "run"])
  , ("M--", sendMessage MirrorShrink)
  , ("M-=", sendMessage MirrorExpand)
  , ("M-<Tab>", toggleWS' ["NSP"])
  -- , ("M-n", moveTo Next NonEmptyWS)
  -- , ("M-p", moveTo Prev NonEmptyWS)
  -- , ("M-S-n", shiftTo Next NonEmptyWS)
  -- , ("M-S-p", shiftTo Prev NonEmptyWS)
  , ("M-S-t", sinkAll)
  , ("M-s", scratchpadSpawnActionCustom "st -n scratchpad")
  , ("M-S-q", confirmPrompt promptConfig "exit" $ io (exitWith ExitSuccess))
  -- Brightness
  , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "5"])
  , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "5"])
  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]
    where promptConfig = def
            { Prompt.fgColor = "red"
            , Prompt.bgColor = "black"
            , Prompt.position = Prompt.Top
            , Prompt.font = myFont
            , Prompt.promptBorderWidth = 0
            }

myMouse =
  [ ((myModMask, button3), (\w -> focus w >> mouseResizeEdgeWindow (1/2) w))
  ]

myStartupHook = do
  return () -- Do not remove
  checkKeymap myConfig myKeys
  adjustEventInput

myLayoutHook =
  let tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (1/2) []
      tabs = renamed [Replace "Tabs"] $ tabbed shrinkText (Themes.theme myTheme)
  in tall ||| tabs
  & renamed [CutWordsLeft 1] . spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  & smartBorders
  & avoidStruts

myManageHook = manageHook def
  <> manageDocks
  <> scratchpadManageHook (RationalRect (1/4) (1/4) (1/2) (1/2))
  <> insertPosition Below Newer

myHandleEventHook = handleEventHook def
  <> fullscreenEventHook
  <> focusOnMouseMove

myLogHook = logHook def
  <> updatePointer (0.5, 0.5) (0, 0)

myTheme :: Themes.ThemeInfo
myTheme = Themes.xmonadTheme
  { Themes.theme = def
    { Deco.activeColor = "black"
    , Deco.inactiveColor = "#333333"
    , Deco.urgentColor = "#333333"
    , Deco.activeBorderColor = "black"
    , Deco.inactiveBorderColor = "#333333"
    , Deco.urgentBorderColor = "#333333"
    , Deco.activeTextColor = "white"
    , Deco.inactiveTextColor = "#999999"
    , Deco.urgentTextColor = "red"
    , Deco.fontName = myFont
    , Deco.decoHeight = 20
    }
  }

{-# ANN module "HLint: Redundant return" #-}
