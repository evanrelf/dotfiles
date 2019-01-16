import XMonad

myConfig = def
  { terminal = "st"
  , focusFollowsMouse = True
  , borderWidth = 2
  -- , normalBorderColor = ""
  -- , focusedBorderColor = ""
  , startupHook = myStartupHook
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , handleEventHook = myHandleEventHook
  , modMask = mod4Mask
  }

main :: IO ()
main = xmonad myConfig
