#!/bin/sh

xrdb -merge .Xresources

feh --bg-scale ~/.wallpaper.jpg

setxkbmap -option caps:escape
xset r rate 200 20

xmonad --replace &
exec xfce4-session
