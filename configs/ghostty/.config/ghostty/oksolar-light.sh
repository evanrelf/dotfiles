#!/usr/bin/env bash

set -Eeuo pipefail
IFS=$'\n\t'

# OKSolar Light (https://meat.io/oksolar) with pure white background

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

darken() { echo "$1" | hsl l % 80 | hsl s % 150; }

lighten() { echo "$1" | hsl l % 160 | hsl s % 75; }

# light_yellow=$(lighten $yellow)
# light_orange=$(lighten $orange)
# light_red=$(lighten $red)
# light_magenta=$(lighten $magenta)
# light_violet=$(lighten $violet)
light_blue=$(lighten $blue)
# light_cyan=$(lighten $cyan)
# light_green=$(lighten $green)

cat <<EOF
# ./oksolar-light.sh
palette = 0=#$base02
palette = 1=#$red
palette = 2=#$green
palette = 3=#$yellow
palette = 4=#$blue
palette = 5=#$magenta
palette = 6=#$cyan
palette = 7=#$base2
palette = 8=#$base03
palette = 9=#$orange
palette = 10=#$base01
palette = 11=#$base00
palette = 12=#$base0
palette = 13=#$violet
palette = 14=#$base1
palette = 15=#$base3
background = ffffff
foreground = $base03
cursor-color = $base1
selection-background = $light_blue
selection-foreground = $base03
EOF
