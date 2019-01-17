import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Layout.ResizableTile (MirrorResize(..), ResizableTall(..))
import qualified XMonad.Util.EZConfig as EZ
import XMonad.Util.Run (safeSpawn)

main :: IO ()
main = xmonad myConfig

myConfig = def
  { terminal = "st"
  , focusFollowsMouse = True
  , clickJustFocuses = True
  , borderWidth = 1
  , normalBorderColor = "#333333"
  , focusedBorderColor = "#999999"
  , startupHook = myStartupHook
  , layoutHook = myLayoutHook
  -- , manageHook = myManageHook
  -- , handleEventHook = myHandleEventHook
  , modMask = mod4Mask
  } `EZ.additionalKeysP` myKeymap

myKeymap =
  -- Window manager
  [ ("M-p", safeSpawn "rofi" ["-show", "run"])
  , ("M--", sendMessage MirrorShrink)
  , ("M-=", sendMessage MirrorExpand)
  , ("M-<Tab>", toggleWS)
  -- Brightness
  , ("<XF86MonBrightnessDown>", safeSpawn "light" ["-U", "5"])
  , ("<XF86MonBrightnessUp>", safeSpawn "light" ["-A", "5"])
  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%-"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "amixer" ["-q", "sset", "Master", "10%+"])
  , ("<XF86AudioMute>", safeSpawn "amixer" ["-q", "sset", "Master", "toggle"])
  ]

myStartupHook = return () >> EZ.checkKeymap myConfig myKeymap

myLayoutHook = ResizableTall 1 (5/100) (1/2) [] ||| Full -- ||| Mirror tiled'
