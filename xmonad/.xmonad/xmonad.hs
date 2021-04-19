{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Data.Function ((&))
import XMonad
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Layout.Decoration (Theme (..), shrinkText)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.Tabbed (tabbedAlways)
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.Run as Run

main = xmonad $ def
  { terminal = "kitty"
  , modMask = mod4Mask
  , focusFollowsMouse = False
  , clickJustFocuses = True
  , borderWidth = 4
  , normalBorderColor = myInactiveColor
  , focusedBorderColor = myActiveColor
  , workspaces = fmap show [1 .. 10 :: Int]
  -- , startupHook = undefined
  -- , logHook = undefined
  , layoutHook = myLayoutHook
  -- , manageHook = undefined
  , handleEventHook = handleEventHook def <> fullscreenEventHook
  }
  & myRemoveKeys
  & myKeys
  & ewmh


myLayoutHook = tall ||| tabs ||| Full & spaced & smartBorders
  where
    tall = do
      let ratio = 1 / 2
      let resizeIncrement = 3 / 100
      Tall 1 resizeIncrement ratio

    tabs = tabbedAlways shrinkText myTheme

    spaced = do
      let smartBorder = True
      let screenBorder = Border 0 0 0 0
      let screenBorderEnabled = False
      let windowBorder = Border 5 5 5 5
      let windowBorderEnabled = True
      spacingRaw
        smartBorder
        screenBorder
        screenBorderEnabled
        windowBorder
        windowBorderEnabled


myRemoveKeys xconfig = EZConfig.removeKeysP xconfig
  -- Resize viewed windows to the correct size (?)
  [ "M-n"
  -- Focus next window
  , "M-<Tab>"
  -- Focus previous window
  , "M-S-<Tab>"
  -- Launch gmrun
  , "M-S-p"
  ]


myKeys xconfig = EZConfig.additionalKeysP xconfig
  -- Switch to last workspace
  [ ("M-<Tab>", CycleWS.toggleWS)

  -- Brightness
  , ("<XF86MonBrightnessDown>", Run.safeSpawn "light" ["-U", "3"])
  , ("<XF86MonBrightnessUp>", Run.safeSpawn "light" ["-A", "3"])

  -- Volume
  , ("<XF86AudioLowerVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]


myActiveColor = "#5577FF"
myInactiveColor = "#333333"


myTheme = def
  { fontName = "xft:Iosevka Term SS08:style=Bold:size=11:antialias=true"
  , decoHeight = 25
  , activeColor = myActiveColor
  , inactiveColor = myInactiveColor
  , activeTextColor = "white"
  , inactiveTextColor = "white"
  , activeBorderWidth = 0
  , inactiveBorderWidth = 0
  , urgentBorderWidth = 0
  }
