#!/bin/sh

[ -f ~/.xprofile ] && . ~/.xprofile

exec xmonad
