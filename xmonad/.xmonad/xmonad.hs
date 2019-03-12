import Data.Function ((&))
import Data.Maybe (isJust)
import System.Exit (ExitCode(..), exitWith)
import XMonad
import XMonad.Config.Xfce (xfceConfig)
import qualified XMonad.Prompt as Prompt
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet (RationalRect(..), Workspace(..), stack, swapMaster)
-- Actions
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), moveTo, shiftTo, toggleWS')
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
-- Hooks
import qualified XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.InsertPosition (Focus(..), Position(..), insertPosition)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
-- Layouts
import qualified XMonad.Layout.Decoration as Deco
import XMonad.Layout.Master (mastered)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.StateFull (focusTracking)
import XMonad.Layout.Tabbed (shrinkText, tabbed)
-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap, removeKeysP)
import XMonad.Util.Loggers (logCmd)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionCustom)
import qualified XMonad.Util.Themes as Themes

-- TODO:
-- * Disable mouse snapping when resizing window with mouse
-- * Format xmobar (brightness, volume, battery, wifi, date, time)

main :: IO ()
main = do
  stalonetray <- spawnPipe "stalonetray"
  xmobar myConfig >>= xmonad

xmobar = DL.statusBar "xmobar" pp toggleStrutsKey
  where
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
    pp = DL.xmobarPP
      { DL.ppHidden = (\w -> if w == "NSP" then "" else w)
      , DL.ppCurrent = DL.xmobarColor "white" "" . DL.wrap "[" "]"
      , DL.ppTitle = const ""
      }

myConfig = xfceConfig
  { terminal = "kitty"
  , workspaces = show <$> [1..9]
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
  & (flip removeKeysP) myRemoveKeys
  & (flip additionalKeysP) myAdditionalKeys
  & (flip additionalMouseBindings) myMouse
  & docks
  & ewmh

myModMask = mod4Mask

myFont = "xft:PragmataPro Liga:style=Regular:size=12:antialias=true"

myRemoveKeys =
  -- Rebound
  [ "M-S-<Return>" -- Spawn terminal
  , "M-S-c" -- Kill window
  , "M-S-<Tab>" -- Focus previous window
  ]

myAdditionalKeys =
  -- Rebinds
  [ ("M-<Return>", spawn $ terminal myConfig)
  , ("M-S-m", windows swapMaster)
  , ("M-c", kill)
  -- Windows
  , ("M-S-t", sinkAll)
  , ("M--", sendMessage MirrorShrink)
  , ("M-=", sendMessage MirrorExpand)
  -- Workspaces
  , ("M-n", moveTo Next $ WSIs (return nonEmptyWS'))
  , ("M-p", moveTo Prev $ WSIs (return nonEmptyWS'))
  , ("M-S-n", shiftTo Next $ WSIs (return anyWS'))
  , ("M-S-p", shiftTo Prev $ WSIs (return anyWS'))
  , ("M-<Tab>", toggleWS' ["NSP"])
  -- , ("M-S-q", confirmPrompt promptConfig "exit" $ io (exitWith ExitSuccess))
  , ("M-S-s", safeSpawn "xfce4-screenshooter" [])
  , ("M-S-q", safeSpawn "xfce4-session-logout" [])
  -- Apps
  -- , ("M-o f", safeSpawn "firefox" [])
  -- , ("M-o n", safeSpawn "nautilus" [])
  , ("M-/", safeSpawn "rofi" ["-show", "run"])
  , ("M-S-/", safeSpawn "rofi" ["-show", "drun"])
  , ("M-s", scratchpadSpawnActionCustom "kitty --name scratchpad")
  -- Brightness
  , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "3"])
  , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "3"])
  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]
    where nonEmptyWS' :: WindowSpace -> Bool
          nonEmptyWS' (Workspace "NSP" _ _) = False
          nonEmptyWS' ws = isJust . stack $ ws

          anyWS' :: WindowSpace -> Bool
          anyWS' (Workspace "NSP" _ _) = False
          anyWS' _ = True

          promptConfig = def
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
  checkKeymap myConfig myAdditionalKeys
  adjustEventInput

myLayoutHook =
  let tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (1/2) []
      -- tallTabs = renamed [Replace "Tall w/ Tabs"] $ mastered (1/20) (1/2) $ focusTracking $ tabbed shrinkText (Themes.theme myTheme)
      tabs = renamed [Replace "Tabs"] $ focusTracking $ tabbed shrinkText (Themes.theme myTheme)
  in tall ||| tabs
  & renamed [CutWordsLeft 1] . spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  & smartBorders
  & avoidStruts

myManageHook = manageHook xfceConfig
  <> manageDocks
  <> scratchpadManageHook (RationalRect (1/4) (1/4) (1/2) (1/2))
  <> insertPosition Below Newer

myHandleEventHook = handleEventHook def
  <> fullscreenEventHook
  <> focusOnMouseMove

myLogHook = logHook def
  -- <> updatePointer (0.5, 0.5) (0, 0)

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
    , Deco.decoHeight = 30
    }
  }

{-# ANN module "HLint: Redundant return" #-}
