-- Miscellaneous
import Data.Function ((&))
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.StackSet (RationalRect(..), Workspace(..), stack, swapMaster)
import qualified XMonad.StackSet as W
-- Actions
import XMonad.Actions.CycleWS (Direction1D(..), WSType(..), doTo, moveTo, shiftTo, toggleWS)
import XMonad.Actions.FlexibleResize (mouseResizeEdgeWindow)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.SwapWorkspaces (Direction1D(..), swapTo)
import XMonad.Actions.UpdateFocus (adjustEventInput, focusOnMouseMove)
import XMonad.Actions.UpdatePointer (updatePointer)
-- Hooks
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
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
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.Util.Themes as Themes
import XMonad.Util.WindowProperties (Property(..), focusedHasProperty)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

main = xmonad myConfig

myConfig = desktopConfig
  -- { terminal = "/home/evanrelf/.config/kitty/launch"
  { terminal = "alacritty"
  , workspaces = show <$> [1..10]
  , focusFollowsMouse = True
  , clickJustFocuses = True
  , borderWidth = 2
  , normalBorderColor = "#333333"
  , focusedBorderColor = "#999999"
  , startupHook = myStartupHook <> startupHook desktopConfig
  , layoutHook = myLayoutHook
  , manageHook = myManageHook <> manageHook desktopConfig
  , handleEventHook = myHandleEventHook <> handleEventHook desktopConfig
  , logHook = myLogHook <> logHook desktopConfig
  , modMask = myModMask
  }
  & flip removeKeysP myRemoveKeys
  & flip additionalKeysP myKeys
  & flip additionalMouseBindings myMouse
  & docks

myModMask = mod4Mask

myFont = "xft:Roboto:style=Regular:size=14:antialias=true"

myRemoveKeys =
  -- Rebound
  [ "M-S-<Return>" -- Spawn terminal
  , "M-S-c" -- Kill window
  , "M-S-<Tab>" -- Focus previous window
  ]

myKeys =
  let moveTo' dir t = doTo dir t getSortByIndex (windows . W.view) in
  -- Windows
  [ ("M-S-m", windows swapMaster)
  , ("M-c", kill)

  -- Workspaces
  , ("M-n", moveTo Next NonEmptyWS)
  , ("M-p", moveTo Prev NonEmptyWS)
  , ("M-S-n", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS)
  , ("M-S-p", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS)
  , ("M-M1-n", swapTo Next)
  , ("M-M1-p", swapTo Prev)
  , ("M-<Tab>", toggleWS)
  -- , ("M-S-q", safeSpawn "xfce4-session-logout" [])
  , ("M-S-q", io exitSuccess)

  -- Apps
  , ("M-<Return>", safeSpawn (terminal myConfig) [])
  -- , ("M-<Return>", do
  --     kitty <- focusedHasProperty $ ClassName "kitty"
  --     if kitty then
  --       -- sendKey (controlMask .|. shiftMask) xK_n
  --       safeSpawn (terminal myConfig) []
  --     else
  --       safeSpawn (terminal myConfig) []
  --   )
  , ("M-S-<Return>", safeSpawn "kitty" [])
  , ("M-/", safeSpawn "rofi" ["-show", "run"])
  , ("M-S-/", safeSpawn "rofi" ["-show", "drun"])

  -- Brightness
  , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "3"])
  , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "3"])

  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]
  <>
  -- Add 10th workspace
  [ ("M-" <> mod <> [key], (windows . action) tag)
    | (tag, key) <- zip (workspaces myConfig) "1234567890"
    , (mod, action) <- [ ("", W.greedyView), ("S-", W.shift) ]
  ]

myMouse =
  [ ((myModMask, button3), \w -> focus w >> mouseResizeEdgeWindow (1/2) w)
  ]

myStartupHook = do
  return () -- Do not remove
  checkKeymap myConfig myKeys
  adjustEventInput
  spawnOnce "~/.config/polybar/launch"

myLayoutHook =
  let tall = renamed [Replace "Tall"] $ ResizableTall 1 (1/20) (1/2) []
      tabs = renamed [Replace "Tabs"] $ focusTracking $ tabbed shrinkText (Themes.theme myTheme)
  in tall ||| tabs -- ||| Full
  & renamed [CutWordsLeft 1] . spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  & smartBorders
  & avoidStruts

myManageHook = manageDocks <> insertPosition Below Newer

myHandleEventHook = focusOnMouseMove <> fullscreenEventHook

myLogHook = updatePointer (0.5, 0.5) (0, 0)

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

{-# ANN module "HLint: ignore Redundant return" #-}
