#!/bin/sh

xrdb -merge .Xresources &
xsetroot -cursor_name left_ptr &
xset r rate 200 20 &
xinput --set-prop "Synaptics TM3075-002" "libinput Natural Scrolling Enabled" 1 &
xinput --set-prop "Synaptics TM3075-002" "libinput Tapping Enabled" 1 &
xinput --set-prop "Synaptics TM3075-002" "libinput Tapping Drag Enabled" 0 &
xinput --set-prop "Synaptics TM3075-002" "libinput Disable While Typing Enabled" 0 &
xinput --set-prop "Synaptics TM3075-002" "libinput Accel Speed" 0.3 &
redshift &
unclutter --exclude-root --fork
feh --bg-tile ~/.wallpapers/current &
exec xmonad
