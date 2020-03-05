evaluate-commands %sh{
  black=rgb:565575
  brightblack=rgb:100e23

  red=rgb:ff8080
  brightred=rgb:ff5458

  green=rgb:95ffa4
  brightgreen=rgb:62d196

  yellow=rgb:ffe9aa
  brightyellow=rgb:ffb378

  blue=rgb:91ddff
  brightblue=rgb:65b2ff

  magenta=rgb:c991e1
  brightmagenta=rgb:906cff

  cyan=rgb:aaffe4
  brightcyan=rgb:63f2f1

  white=rgb:cbe3e7
  brightwhite=rgb:a6b3cc

  foreground=$white
  background=rgb:1e1c31
  selection=rgb:393848

  c() {
    echo "$1,$2"
  }

  printf "%s\n" "
  face global attribute          $(c default        default)
  face global block              $(c default        default)
  face global bold               $(c default        default)
  face global builtin            $(c default        default)
  face global bullet             $(c default        default)
  face global comment            $(c $black         default)
  face global error              $(c $red           default)
  face global function           $(c default        default)
  face global header             $(c default        default)
  face global identifier         $(c default        default)
  face global italic             $(c default        default)
  face global keyword            $(c default        default)
  face global link               $(c default        default)
  face global list               $(c default        default)
  face global meta               $(c default        default)
  face global module             $(c default        default)
  face global mono               $(c default        default)
  face global operator           $(c $cyan          default)
  face global string             $(c $yellow        default)
  face global title              $(c default        default)
  face global type               $(c default        default)
  face global value              $(c default        default)
  face global variable           $(c default        default)

  face global Default            $(c default        default)

  face global Error              $(c $red           default)

  face global LineNumberCursor   $(c default        default)
  face global LineNumbers        $(c $black         default)
  face global LineNumbersWrapped $(c default        default)
  face global BufferPadding      $(c $black         default)

  face global MatchingChar       $(c default        $black)

  face global MenuBackground     $(c $brightblack   $foreground)
  face global MenuForeground     $(c default        $brightblack)
  face global MenuInfo           $(c $black         default)
  face global Information        $(c default        $brightblack)

  face global PrimaryCursor      $(c $background    $yellow)
  face global PrimaryCursorEol   $(c $background    $brightyellow)
  face global PrimarySelection   $(c default        $selection)
  face global SecondaryCursor    $(c $background    $foreground)
  face global SecondaryCursorEol $(c $background    $brightwhite)
  face global SecondarySelection $(c default        $selection)
  face global StatusCursor       $(c $background    $yellow)

  face global StatusLine         $(c default        default)
  face global StatusLineInfo     $(c default        $brightblack)
  face global StatusLineMode     $(c default        $brightblack)
  face global StatusLineValue    $(c $background    $yellow)
  face global Prompt             $(c default        default)

  face global Whitespace         $(c default        default)
  face global WrapMarker         $(c default        default)
  "
}
