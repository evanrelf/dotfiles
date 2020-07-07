#!/bin/sh

set -e

command -v amixer >/dev/null 2>&1 \
  || (echo "can't find amixer" >&2; exit 1)

command -v grep >/dev/null 2>&1 \
  || (echo "can't find grep" >&2; exit 1)

command -v head >/dev/null 2>&1 \
  || (echo "can't find head" >&2; exit 1)

command -v light >/dev/null 2>&1 \
  || (echo "can't find light" >&2; exit 1)

[ -f "/sys/class/power_supply/BAT0/capacity" ] \
  || (echo "can't find battery capacity" >&2; 1)

command -v date >/dev/null 2>&1 \
  || (echo "can't find date" >&2; exit 1)

volume="Volume: $(amixer get Master | grep --extended-regexp --only-matching '[0-9]+%' | head -1)"
brightness="Brightness: $(light -G | cut -d '.' -f 1)%"
battery="Battery: $(cat /sys/class/power_supply/BAT0/capacity)%"
date=$(date +'%Y-%m-%d %l:%M %p')

echo "$volume | $brightness | $battery | $date"
