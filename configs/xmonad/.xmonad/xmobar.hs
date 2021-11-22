module Main (main) where

import Xmobar


main :: IO ()
main = xmobar config


config :: Config
config = defaultConfig
  { font = "xft:Iosevka Term SS08-10"
  , bgColor = "#777777"
  , fgColor = "#111111"
  , template = "%uname% } %hello% { %date%"
  , commands =
      [ Run (Com "uname" ["-s", "-r"] "" 36000)
      , Run (Date "%Y-%m-%dT%H:%M:%S%Z" "date" 10)
      , Run HelloWorld
      ]
  }


data HelloWorld = HelloWorld
  deriving stock (Read, Show)


instance Exec HelloWorld where
  alias _ = "hello"
  run _ = pure "<fc=red>Hello world</fc>"
