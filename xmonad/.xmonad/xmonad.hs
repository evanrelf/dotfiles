module Main (main) where

import XMonad (XConfig (..), def, mod4Mask, xmonad)
import qualified XMonad.Actions.CycleWS as CycleWS
import qualified XMonad.Util.EZConfig as EZConfig
import qualified XMonad.Util.Run as Run


main :: IO ()
main = xmonad
  . myKeys
  . myRemoveKeys
  . myConfig
  $ def


myConfig :: XConfig l -> XConfig l
myConfig xconfig = xconfig
  { terminal = "kitty"
  , modMask = mod4Mask
  , focusFollowsMouse = False
  , clickJustFocuses = True
  , borderWidth = 4
  , normalBorderColor = "#777777"
  , focusedBorderColor = "#BB77FF"
  , workspaces = fmap show [1 .. 10 :: Int]
  -- , startupHook = undefined
  -- , logHook = undefined
  -- , layoutHook = undefined
  -- , manageHook = undefined
  -- , handleEventHook = undefined
  }


myRemoveKeys :: XConfig l -> XConfig l
myRemoveKeys xconfig = EZConfig.removeKeysP xconfig
  -- Focus previous window
  [ "M-S-<Tab>"
  ]


myKeys :: XConfig l -> XConfig l
myKeys xconfig = EZConfig.additionalKeysP xconfig
  -- Launching
  [ ("M-d", Run.safeSpawnProg "dmenu_run")
  -- Workspaces
  , ("M-<Tab>", CycleWS.toggleWS)
  -- Brightness
  , ("<XF86MonBrightnessDown>", Run.safeSpawn "light" ["-U", "3"])
  , ("<XF86MonBrightnessUp>", Run.safeSpawn "light" ["-A", "3"])
  -- Volume
  , ("<XF86AudioLowerVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", Run.safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]
