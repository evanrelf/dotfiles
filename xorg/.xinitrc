#!/bin/sh

xrdb -merge .Xresources
# caps -> esc
# sudo setkeycodes 3a 1
# alt -> cmd
# sudo setkeycodes 38 125
# cmd -> alt
# sudo setkeycodes e05b 56
feh --bg-scale ~/.wallpaper.jpg
xset r rate 200 20
exec xmonad
