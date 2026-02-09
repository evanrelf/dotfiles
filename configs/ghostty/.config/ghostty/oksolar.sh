#!/usr/bin/env bash

# shellcheck disable=SC2034

set -Eeuo pipefail
IFS=$'\n\t'

# https://meat.io/oksolar

base03=002d38
base02=093946
base01=5b7279
base00=657377
base0=98a8a8
base1=8faaab
base2=f1e9d2
base3=fbf7ef
yellow=ac8300
orange=d56500
red=f23749
magenta=dd459d
violet=7d80d1
blue=2b90d8
cyan=259d94
green=819500

darken() { echo "$1" | hsl l%80 s%150; }

lighten() { echo "$1" | hsl l%160 s%75; }

light_yellow=$(lighten $yellow)
light_orange=$(lighten $orange)
light_red=$(lighten $red)
light_magenta=$(lighten $magenta)
light_violet=$(lighten $violet)
light_blue=$(lighten $blue)
light_cyan=$(lighten $cyan)
light_green=$(lighten $green)

if [ "$#" -ne 1 ]; then
  echo "usage: ./oksolar.sh <light|light-nobright|dark|dark-nobright>"
  exit 1
fi

oksolar_theme=$1

echo "# ./oksolar.sh $oksolar_theme"

cat <<EOF
palette = 0=#$base02
palette = 1=#$red
palette = 2=#$green
palette = 3=#$yellow
palette = 4=#$blue
palette = 5=#$magenta
palette = 6=#$cyan
palette = 7=#$base2
EOF

if echo "$oksolar_theme" | rg --quiet '\-nobright$'; then
  cat <<EOF
palette = 8=#$base02
palette = 9=#$red
palette = 10=#$green
palette = 11=#$yellow
palette = 12=#$blue
palette = 13=#$magenta
palette = 14=#$cyan
palette = 15=#$base2
EOF
else
  cat <<EOF
palette = 8=#$base03
palette = 9=#$orange
palette = 10=#$base01
palette = 11=#$base00
palette = 12=#$base0
palette = 13=#$violet
palette = 14=#$base1
palette = 15=#$base3
EOF
fi

if echo "$oksolar_theme" | rg --quiet '^light\b'; then
  cat <<EOF
background = ffffff
foreground = $base03
cursor-color = $base1
selection-background = $light_blue
selection-foreground = $base03
EOF
else
  cat <<EOF
background = $base03
foreground = $base2
cursor-color = $base1
selection-background = $base02
selection-foreground = $base3
EOF
fi
