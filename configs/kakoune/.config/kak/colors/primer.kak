evaluate-commands %sh{

# All colors

export black="rgb:1b1f23"

export white="rgb:ffffff"

export gray_000="rgb:fafbfc"
export gray_100="rgb:f6f8fa"
export gray_200="rgb:e1e4e8"
export gray_300="rgb:d1d5da"
export gray_400="rgb:959da5"
export gray_500="rgb:6a737d"
export gray_600="rgb:586069"
export gray_700="rgb:444d56"
export gray_800="rgb:2f363d"
export gray_900="rgb:24292e"

export blue_000="rgb:f1f8ff"
export blue_100="rgb:dbedff"
export blue_200="rgb:c8e1ff"
export blue_300="rgb:79b8ff"
export blue_400="rgb:2188ff"
export blue_500="rgb:0366d6"
export blue_600="rgb:005cc5"
export blue_700="rgb:044289"
export blue_800="rgb:032f62"
export blue_900="rgb:05264c"

export green_000="rgb:f0fff4"
export green_100="rgb:dcffe4"
export green_200="rgb:bef5cb"
export green_300="rgb:85e89d"
export green_400="rgb:34d058"
export green_500="rgb:28a745"
export green_600="rgb:22863a"
export green_700="rgb:176f2c"
export green_800="rgb:165c26"
export green_900="rgb:144620"

export yellow_000="rgb:fffdef"
export yellow_100="rgb:fffbdd"
export yellow_200="rgb:fff5b1"
export yellow_300="rgb:ffea7f"
export yellow_400="rgb:ffdf5d"
export yellow_500="rgb:ffd33d"
export yellow_600="rgb:f9c513"
export yellow_700="rgb:dbab09"
export yellow_800="rgb:b08800"
export yellow_900="rgb:735c0f"

export orange_000="rgb:fff8f2"
export orange_100="rgb:ffebda"
export orange_200="rgb:ffd1ac"
export orange_300="rgb:ffab70"
export orange_400="rgb:fb8532"
export orange_500="rgb:f66a0a"
export orange_600="rgb:e36209"
export orange_700="rgb:d15704"
export orange_800="rgb:c24e00"
export orange_900="rgb:a04100"

export red_000="rgb:ffeef0"
export red_100="rgb:ffdce0"
export red_200="rgb:fdaeb7"
export red_300="rgb:f97583"
export red_400="rgb:ea4a5a"
export red_500="rgb:d73a49"
export red_600="rgb:cb2431"
export red_700="rgb:b31d28"
export red_800="rgb:9e1c23"
export red_900="rgb:86181d"

export purple_000="rgb:f5f0ff"
export purple_100="rgb:e6dcfd"
export purple_200="rgb:d1bcf9"
export purple_300="rgb:b392f0"
export purple_400="rgb:8a63d2"
export purple_500="rgb:6f42c1"
export purple_600="rgb:5a32a3"
export purple_700="rgb:4c2889"
export purple_800="rgb:3a1d6e"
export purple_900="rgb:29134e"

export pink_000="rgb:ffeef8"
export pink_100="rgb:fedbf0"
export pink_200="rgb:f9b3dd"
export pink_300="rgb:f692ce"
export pink_400="rgb:ec6cb9"
export pink_500="rgb:ea4aaa"
export pink_600="rgb:d03592"
export pink_700="rgb:b93a86"
export pink_800="rgb:99306f"
export pink_900="rgb:6d224f"

# Background colors

export bg_gray_light="$gray_000"
export bg_gray="$gray_100"
export bg_gray_dark="$gray_900"
export bg_blue_light="$blue_000"
export bg_blue="$blue_500"
export bg_green_light="$green_100"
export bg_green="$green_500"
export bg_purple_light="$purple_000"
export bg_purple="$purple_500"
export bg_yellow_light="$yellow_200"
export bg_yellow="$yellow_500"
export bg_yellow_dark="$yellow_700"
export bg_orange="$orange_700"
export bg_red_light="$red_100"
export bg_red="$red_500"
export bg_pink="$pink_500"

# Text colors

export text_gray_light="$gray_500"
export text_gray="$gray_600"
export text_gray_dark="$gray_900"
export text_blue="$blue_500"
export text_green="$green_500"
export text_purple="$purple_500"
export text_yellow="$yellow_800"
export text_orange_light="$orange_600"
export text_orange="$orange_900"
export text_red="$red_600"
export text_pink="$pink_500"

c() {
  fg="$1"
  bg="$2"
  attrs="$3"

  [ -n "$fg" ] && printf "%s" "$fg"
  [ -n "$bg" ] && printf "%s" ",$bg"
  [ -n "$attrs" ] && printf "%s" "+$attrs"
}

printf "%s\n" "
declare-option str column_color 'rgb:f6f8fa'

# Code
set-face global title              $(c $text_orange       default)
set-face global header             $(c $text_red          default b)
set-face global bold               $(c $text_orange_light default b)
set-face global italic             $(c $text_pink         default i)
set-face global mono               $(c $text_purple       default)
set-face global block              $(c default            default)
set-face global link               $(c $text_blue         default u)
set-face global bullet             $(c default            default)
set-face global list               $(c default            default)

# Markup
set-face global value              $(c $text_blue         default)
set-face global type               $(c $text_blue         default)
set-face global variable           $(c $text_blue         default)
set-face global module             $(c $text_purple       default)
set-face global function           $(c $text_purple       default)
set-face global string             $(c $text_yellow       default)
set-face global keyword            $(c $text_red          default b)
set-face global operator           $(c $text_orange_light default)
set-face global attribute          $(c $text_green        default)
set-face global comment            $(c $text_gray_light   default i)
set-face global documentation      $(c $text_gray         default i)
set-face global meta               $(c $text_orange       default)
set-face global builtin            $(c $text_red          default)

# Deprecated?
set-face global error              $(c $black             $bg_red_light)
set-face global identifier         $(c default            default)

# Interface
set-face global Default            $(c $black             $white)

set-face global Error              $(c $black             $bg_red_light)

set-face global LineNumberCursor   $(c $text_gray         $bg_gray b)
set-face global LineNumbers        $(c $text_gray_light   $bg_gray)
set-face global LineNumbersWrapped $(c $text_gray_light   $bg_gray)
set-face global BufferPadding      $(c default            default)

set-face global MatchingChar       $(c default            $bg_green_light)

set-face global MenuBackground     $(c $black             $bg_gray)
set-face global MenuForeground     $(c $white             $bg_blue)
set-face global MenuInfo           $(c default            default)
set-face global Information        $(c $black             $bg_blue_light)

set-face global PrimaryCursor      $(c $black             $bg_yellow)
set-face global PrimaryCursorEol   $(c default            $bg_orange)
set-face global PrimarySelection   $(c default            $bg_yellow_light)
set-face global SecondaryCursor    $(c $bg_red_light      $bg_pink)
set-face global SecondaryCursorEol $(c $bg_red_light      $bg_red)
set-face global SecondarySelection $(c default            $bg_red_light)
set-face global StatusCursor       $(c default            $bg_gray_dark)

set-face global StatusLine         $(c $black             $bg_gray)
set-face global StatusLineInfo     $(c default            $bg_yellow_light)
set-face global StatusLineMode     $(c $text_blue         $bg_gray)
set-face global StatusLineValue    $(c $text_pink         $bg_gray)
set-face global Prompt             $(c default            default)

set-face global Whitespace         $(c default            default)
set-face global WrapMarker         $(c default            default)
"
}
