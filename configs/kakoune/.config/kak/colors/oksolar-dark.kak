evaluate-commands %sh{

# Based on OKSolar (https://meat.io/oksolar)

hsl() { sed 's/^rgb:/#/' | command hsl "$@" | sed 's/^#/rgb:/'; }

base03="rgb:002d38"
base02="rgb:093946"
base01="rgb:5b7279"
base00="rgb:657377"
base0="rgb:98a8a8"
base1="rgb:8faaab"
base2="rgb:f1e9d2"
base3="rgb:fbf7ef"

yellow="rgb:ac8300"
orange="rgb:d56500"
red="rgb:f23749"
magenta="rgb:dd459d"
violet="rgb:7d80d1"
blue="rgb:2b90d8"
cyan="rgb:259d94"
green="rgb:819500"

black="$base03"
white="$base2"

darken() { echo "$1" | hsl l % 80 | hsl s % 150; }

lighten() { echo "$1" | hsl l % 160 | hsl s % 75; }

# light_yellow=$(lighten $yellow)
# light_orange=$(lighten $orange)
# light_red=$(lighten $red)
# light_magenta=$(lighten $magenta)
# light_violet=$(lighten $violet)
# light_blue=$(lighten $blue)
# light_cyan=$(lighten $cyan)
# light_green=$(lighten $green)

light_yellow="rgb:fae6b9"
light_orange="rgb:fce1d3"
light_red="rgb:fae0df"
light_magenta="rgb:f7dfea"
light_violet="rgb:e5e6f5"
light_blue="rgb:d7e9fa"
light_cyan="rgb:bef3ed"
light_green="rgb:e0f2a3"

c() {
  local fg="$1"; [ -n "$fg" ] && printf "%s" "$fg"
  local bg="$2"; [ -n "$bg" ] && printf "%s" ",$bg"
  local attrs="$3"; [ -n "$attrs" ] && printf "%s" "+$attrs"
}

printf "%s\n" "
declare-option str column_color $base02

set-face global Default            $(c $white $black)

# Code
set-face global title              $(c default default)
set-face global header             $(c default default b)
set-face global bold               $(c default default b)
set-face global italic             $(c default default i)
set-face global mono               $(c $yellow default)
set-face global block              $(c default default)
set-face global link               $(c $blue default u)
set-face global bullet             $(c default default)
set-face global list               $(c default default)

# # Markup (colorful)
# set-face global value              $(c $blue default)
# set-face global type               $(c $blue default)
# set-face global variable           $(c $blue default)
# set-face global module             $(c $purple default)
# set-face global function           $(c $purple default)
# set-face global string             $(c $yellow default)
# set-face global keyword            $(c $red default b)
# set-face global operator           $(c $orange default)
# set-face global attribute          $(c $green default)
# set-face global comment            $(c $base0 default i)
# set-face global documentation      $(c $base0 default i)
# set-face global meta               $(c $orange default)
# set-face global builtin            $(c $red default)

# Markup (minimal)
set-face global value              $(c $blue default)
hook global BufSetOption filetype=rust %{
  set-face buffer value $(c default default)
}
set-face global type               $(c default default)
set-face global variable           $(c default default)
set-face global module             $(c default default)
set-face global function           $(c default default)
set-face global string             $(c $yellow default)
set-face global keyword            $(c default default)
set-face global operator           $(c default default)
set-face global attribute          $(c default default)
set-face global comment            $(c $base0 default)
set-face global documentation      $(c $base0 default)
set-face global meta               $(c default default)
set-face global builtin            $(c default default)

# Interface
set-face global Whitespace         $(c $base2 default f)
set-face global WrapMarker         $(c default default)
set-face global Error              $(c default $light_red)
set-face global DiagnosticError    $(c default $light_red)
set-face global DiagnosticWarning  $(c default $light_violet)
set-face global LineNumberCursor   $(c $base00 default b)
set-face global LineNumbers        $(c $base0 default)
set-face global LineNumbersWrapped $(c $white default)
set-face global BufferPadding      $(c $base0 default)
set-face global MatchingChar       $(c default $light_yellow)
set-face global PrimaryCursor      $(c $white $blue fg)
set-face global PrimaryCursorEol   $(c default $blue g)
set-face global PrimarySelection   $(c default $light_blue g)
set-face global SecondaryCursor    $(c $white $yellow fg)
set-face global SecondaryCursorEol $(c default $yellow g)
set-face global SecondarySelection $(c default $light_yellow g)
set-face global StatusCursor       $(c default $black g)
set-face global MenuBackground     $(c default $base3)
set-face global MenuForeground     $(c $white $blue)
set-face global MenuInfo           $(c default default)
set-face global Information        $(c default $base3)
set-face global InlineInformation  $(c default $base3)
set-face global StatusLine         $(c default $base3)
set-face global StatusLineInfo     $(c default default)
set-face global StatusLineMode     $(c default $base2)
set-face global StatusLineValue    $(c default $base2)
set-face global Prompt             $(c default $base2)

# Deprecated?
set-face global error              $(c $red $light_green F)
set-face global identifier         $(c $red $light_green F)
"
}
