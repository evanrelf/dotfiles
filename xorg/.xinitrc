#!/bin/sh

# shellcheck source=/dev/null
[ -f ~/.xprofile ] && . ~/.xprofile

exec xmonad
