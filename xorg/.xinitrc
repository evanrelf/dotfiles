#!/bin/sh

xrdb -merge .Xresources

feh --bg-scale ~/.wallpaper.jpg

setxkbmap -option caps:escape
xset r rate 200 30

exec xmonad
