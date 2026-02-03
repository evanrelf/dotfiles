evaluate-commands %sh{

# https://stephango.com/flexoki

white="rgb:FFFFFF"
paper="rgb:FFFCF0"
base_50="rgb:F2F0E5"
base_100="rgb:E6E4D9"
base_150="rgb:DAD8CE"
base_200="rgb:CECDC3"
base_300="rgb:B7B5AC"
base_400="rgb:9F9D96"
base_500="rgb:878580"
base_600="rgb:6F6E69"
base_700="rgb:575653"
base_800="rgb:403E3C"
base_850="rgb:343331"
base_900="rgb:282726"
base_950="rgb:1C1B1A"
black="rgb:100F0F"

red_50="rgb:FFE1D5"
red_100="rgb:FFCABB"
red_150="rgb:FDB2A2"
red_200="rgb:F89A8A"
red_300="rgb:E8705F"
red_400="rgb:D14D41"
red_500="rgb:C03E35"
red_600="rgb:AF3029"
red_700="rgb:942822"
red_800="rgb:6C201C"
red_850="rgb:551B18"
red_900="rgb:3E1715"
red_950="rgb:261312"

orange_50="rgb:FFE7CE"
orange_100="rgb:FED3AF"
orange_150="rgb:FCC192"
orange_200="rgb:F9AE77"
orange_300="rgb:EC8B49"
orange_400="rgb:DA702C"
orange_500="rgb:CB6120"
orange_600="rgb:BC5215"
orange_700="rgb:9D4310"
orange_800="rgb:71320D"
orange_850="rgb:59290D"
orange_900="rgb:40200D"
orange_950="rgb:27180E"

yellow_50="rgb:FAEEC6"
yellow_100="rgb:F6E2A0"
yellow_150="rgb:F1D67E"
yellow_200="rgb:ECCB60"
yellow_300="rgb:DFB431"
yellow_400="rgb:D0A215"
yellow_500="rgb:BE9207"
yellow_600="rgb:AD8301"
yellow_700="rgb:8E6B01"
yellow_800="rgb:664D01"
yellow_850="rgb:503D02"
yellow_900="rgb:3A2D04"
yellow_950="rgb:241E08"

green_50="rgb:EDEECF"
green_100="rgb:DDE2B2"
green_150="rgb:CDD597"
green_200="rgb:BEC97E"
green_300="rgb:A0AF54"
green_400="rgb:879A39"
green_500="rgb:768D21"
green_600="rgb:66800B"
green_700="rgb:536907"
green_800="rgb:3D4C07"
green_850="rgb:313D07"
green_900="rgb:252D09"
green_950="rgb:1A1E0C"

cyan_50="rgb:DDF1E4"
cyan_100="rgb:BFE8D9"
cyan_150="rgb:A2DECE"
cyan_200="rgb:87D3C3"
cyan_300="rgb:5ABDAC"
cyan_400="rgb:3AA99F"
cyan_500="rgb:2F968D"
cyan_600="rgb:24837B"
cyan_700="rgb:1C6C66"
cyan_800="rgb:164F4A"
cyan_850="rgb:143F3C"
cyan_900="rgb:122F2C"
cyan_950="rgb:101F1D"

blue_50="rgb:E1ECEB"
blue_100="rgb:C6DDE8"
blue_150="rgb:ABCFE2"
blue_200="rgb:92BFDB"
blue_300="rgb:66A0C8"
blue_400="rgb:4385BE"
blue_500="rgb:3171B2"
blue_600="rgb:205EA6"
blue_700="rgb:1A4F8C"
blue_800="rgb:163B66"
blue_850="rgb:133051"
blue_900="rgb:12253B"
blue_950="rgb:101A24"

purple_50="rgb:F0EAEC"
purple_100="rgb:E2D9E9"
purple_150="rgb:D3CAE6"
purple_200="rgb:C4B9E0"
purple_300="rgb:A699D0"
purple_400="rgb:8B7EC8"
purple_500="rgb:735EB5"
purple_600="rgb:5E409D"
purple_700="rgb:4F3685"
purple_800="rgb:3C2A62"
purple_850="rgb:31234E"
purple_900="rgb:261C39"
purple_950="rgb:1A1623"

magenta_50="rgb:FEE4E5"
magenta_100="rgb:FCCFDA"
magenta_150="rgb:F9B9CF"
magenta_200="rgb:F4A4C2"
magenta_300="rgb:E47DA8"
magenta_400="rgb:CE5D97"
magenta_500="rgb:B74583"
magenta_600="rgb:A02F6F"
magenta_700="rgb:87285E"
magenta_800="rgb:641F46"
magenta_850="rgb:4F1B39"
magenta_900="rgb:39172B"
magenta_950="rgb:24131D"

flexoki_theme=${flexoki_theme:-light}

if [ "$flexoki_theme" = "white" ]; then
  tx="$black"
  tx_2="$base_600"
  tx_3="$base_300"
  ui_3="$base_200"
  ui_2="$base_150"
  ui="$base_100"
  bg_2="$base_50"
  bg="$white"
  red="$red_600"
  orange="$orange_600"
  yellow="$yellow_600"
  green="$green_600"
  cyan="$cyan_600"
  blue="$blue_600"
  purple="$purple_600"
  magenta="$magenta_600"
elif [ "$flexoki_theme" = "light" ]; then
  tx="$black"
  tx_2="$base_600"
  tx_3="$base_300"
  ui_3="$base_200"
  ui_2="$base_150"
  ui="$base_100"
  bg_2="$base_50"
  bg="$paper"
  red="$red_600"
  orange="$orange_600"
  yellow="$yellow_600"
  green="$green_600"
  cyan="$cyan_600"
  blue="$blue_600"
  purple="$purple_600"
  magenta="$magenta_600"
elif [ "$flexoki_theme" = "dark" ]; then
  bg="$black"
  bg_2="$base_950"
  ui="$base_900"
  ui_2="$base_850"
  ui_3="$base_800"
  tx_3="$base_700"
  tx_2="$base_500"
  tx="$base_200"
  red="$red_400"
  orange="$orange_400"
  yellow="$yellow_400"
  green="$green_400"
  cyan="$cyan_400"
  blue="$blue_400"
  purple="$purple_400"
  magenta="$magenta_400"
else
  echo "error: invalid flexoki theme '$flexoki_theme'" >&1
  exit 1
fi

c() {
  local fg="$1"; [ -n "$fg" ] && printf "%s" "$fg"
  local bg="$2"; [ -n "$bg" ] && printf "%s" ",$bg"
  local attrs="$3"; [ -n "$attrs" ] && printf "%s" "+$attrs"
}

printf "%s\n" "
declare-option str column_color $ui

set-face global Default            $(c $tx $bg)

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

# Markup
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
set-face global comment            $(c $tx_3 default)
set-face global documentation      $(c $tx_3 default)
set-face global meta               $(c default default)
set-face global builtin            $(c default default)

# Interface
set-face global Whitespace         $(c $ui_3 default f)
set-face global WrapMarker         $(c default default)
set-face global Error              $(c default $red_100)
set-face global DiagnosticError    $(c default $red_100)
set-face global DiagnosticWarning  $(c default $purple_100)
set-face global LineNumberCursor   $(c $tx_2 default b)
set-face global LineNumbers        $(c $tx_3 default)
set-face global LineNumbersWrapped $(c $red $green_100) # not sure what this is?
set-face global BufferPadding      $(c $tx_3 default)
set-face global MatchingChar       $(c default $yellow_100)
set-face global PrimaryCursor      $(c $paper $blue fg)
set-face global PrimaryCursorEol   $(c default $blue g)
set-face global PrimarySelection   $(c default $blue_100 g)
set-face global SecondaryCursor    $(c $paper $yellow fg)
set-face global SecondaryCursorEol $(c default $yellow g)
set-face global SecondarySelection $(c default $yellow_100 g)
set-face global StatusCursor       $(c default default g)
set-face global MenuBackground     $(c default $ui)
set-face global MenuForeground     $(c $paper $blue)
set-face global MenuInfo           $(c default default)
set-face global Information        $(c default $ui)
set-face global InlineInformation  $(c default $ui)
set-face global StatusLine         $(c default $ui)
set-face global StatusLineInfo     $(c default default)
set-face global StatusLineMode     $(c default $ui_3)
set-face global StatusLineValue    $(c default $ui_3)
set-face global Prompt             $(c default $ui_3)

# TODO
# set-face global DiagnosticHint            $(c $red $green_100)
# set-face global DiagnosticInfo            $(c $red $green_100)
# set-face global DiagnosticTagDeprecated   $(c $red $green_100)
# set-face global DiagnosticTagUnnecessary  $(c $red $green_100)
# set-face global InlayCodeLens             $(c $red $green_100)
# set-face global InlayDiagnosticError      $(c $red $green_100)
# set-face global InlayDiagnosticHint       $(c $red $green_100)
# set-face global InlayDiagnosticInfo       $(c $red $green_100)
# set-face global InlayDiagnosticWarning    $(c $red $green_100)
# set-face global InlayHint                 $(c $red $green_100)
# set-face global LineFlagError             $(c $red $green_100)
# set-face global LineFlagHint              $(c $red $green_100)
# set-face global LineFlagInfo              $(c $red $green_100)
# set-face global LineFlagWarning           $(c $red $green_100)
# set-face global Reference                 $(c $red $green_100)
# set-face global ReferenceBind             $(c $red $green_100)
# set-face global SnippetsNextPlaceholders  $(c $red $green_100)
# set-face global SnippetsOtherPlaceholders $(c $red $green_100)
# set-face global WhitespaceIndent          $(c $red $green_100)

# Deprecated?
set-face global error              $(c $red $green_100 F)
set-face global identifier         $(c $red $green_100 F)
"
}
