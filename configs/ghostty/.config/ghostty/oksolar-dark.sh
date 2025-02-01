#!/usr/bin/env bash

set -Eeuo pipefail
IFS=$'\n\t'

# OKSolar Dark (https://meat.io/oksolar) with pure black background

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

cat <<EOF
# ./oksolar-dark.sh
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
background = $base03
foreground = $base2
cursor-color = $base1
selection-background = $base02
selection-foreground = $base3
EOF
