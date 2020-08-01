{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Data.Function ((&))
import XMonad (XConfig (..), def, mod4Mask, xmonad)
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Layout.Decoration (Theme (..))
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco, shrinkText)
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.Run as Run


main = xmonad $ def
  { terminal = "kitty"
  , modMask = mod4Mask
  , focusFollowsMouse = False
  , clickJustFocuses = True
  , borderWidth = 3
  , normalBorderColor = myInactiveColor
  , focusedBorderColor = myActiveColor
  , workspaces = fmap show [1 .. 10 :: Int]
  -- , startupHook = undefined
  -- , logHook = undefined
  , layoutHook = myLayoutHook
  -- , manageHook = undefined
  -- , handleEventHook = undefined
  }
  & myRemoveKeys
  & myKeys


myLayoutHook = layoutHook def
  & noFrillsDeco shrinkText myTheme


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
  -- Open new Emacs frame
  [ ("M-S-;", Run.safeSpawn "sh" ["-c", "ALTERNATE_EDITOR='' emacsclient -s gui -nc"])
  -- Switch to last workspace
  , ("M-<Tab>", CycleWS.toggleWS)
  -- Brightness
  , ("<XF86MonBrightnessDown>", Run.safeSpawn "light" ["-U", "3"])
  , ("<XF86MonBrightnessUp>", Run.safeSpawn "light" ["-A", "3"])
  -- Volume
  , ("<XF86AudioLowerVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]


myActiveColor = "#1A11A1"
myInactiveColor = "#555555"


myTheme = def
  { fontName = "xft:Terminus:style=Regular:size=10:antialias=true"
  , decoHeight = 10
  , activeColor = myActiveColor
  , inactiveColor = myInactiveColor
  , activeTextColor = myActiveColor
  , inactiveTextColor = myInactiveColor
  , activeBorderWidth = 0
  , inactiveBorderWidth = 0
  , urgentBorderWidth = 0
  }
