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

cat <<EOF
# ./oksolar-light.sh
[colors]
background = "#ffffff"
foreground = "#$base03"
cursor_bg = "#$base1"
cursor_fg = "#$base03"
cursor_border = "#$base1"
selection_bg = "#$base2"
selection_fg = "#$base03"
scrollbar_thumb = "#$base1"
ansi = [
  "#$base02",
  "#$red",
  "#$green",
  "#$yellow",
  "#$blue",
  "#$magenta",
  "#$cyan",
  "#$base2",
]
brights = [
  "#$base03",
  "#$orange",
  "#$base01",
  "#$base00",
  "#$base0",
  "#$violet",
  "#$base1",
  "#$base3",
]
EOF
