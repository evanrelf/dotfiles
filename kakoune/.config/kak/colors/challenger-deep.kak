try %{
  source "%val{config}/colors/challenger-deep-cached.kak"
} catch %{ evaluate-commands %sh{
  black="rgb:565575"
  brightblack="rgb:100e23"

  red="rgb:ff8080"
  brightred="rgb:ff5458"

  green="rgb:95ffa4"
  brightgreen="rgb:62d196"

  yellow="rgb:ffe9aa"
  brightyellow="rgb:ffb378"

  blue="rgb:91ddff"
  brightblue="rgb:65b2ff"

  magenta="rgb:c991e1"
  brightmagenta="rgb:906cff"

  cyan="rgb:aaffe4"
  brightcyan="rgb:63f2f1"

  white="rgb:cbe3e7"
  brightwhite="rgb:a6b3cc"

  foreground="$white"
  background="rgb:1e1c31"
  selection="rgb:393848"

  c() {
    fg="$1"
    bg="$2"
    attrs="$3"

    [ -n "$fg" ] && printf "%s" "$fg"
    [ -n "$bg" ] && printf "%s" ",$bg"
    [ -n "$attrs" ] && printf "%s" "+$attrs"
  }

  mkdir -p "$kak_config/colors"
  printf "%s\n" "
  # Code
  set-face global title              $(c default        default)
  set-face global header             $(c default        default)
  set-face global bold               $(c default        default)
  set-face global italic             $(c default        default)
  set-face global mono               $(c default        default)
  set-face global block              $(c default        default)
  set-face global link               $(c default        default)
  set-face global bullet             $(c default        default)
  set-face global list               $(c default        default)

  # Markup
  set-face global value              $(c $brightyellow  default)
  set-face global type               $(c $blue          default)
  set-face global variable           $(c default        default)
  set-face global module             $(c $green         default)
  set-face global function           $(c default        default)
  set-face global string             $(c $yellow        default)
  set-face global keyword            $(c $magenta       default)
  set-face global operator           $(c $cyan          default)
  set-face global attribute          $(c default        default)
  set-face global comment            $(c $black         default)
  set-face global documentation      $(c default        default)
  set-face global meta               $(c $red           default)
  set-face global builtin            $(c default        default)

  # Deprecated?
  set-face global error              $(c $red           default)
  set-face global identifier         $(c default        default)

  # Interface
  set-face global Default            $(c default        default)

  set-face global Error              $(c $red           default)

  set-face global LineNumberCursor   $(c default        default)
  set-face global LineNumbers        $(c $black         default)
  set-face global LineNumbersWrapped $(c default        default)
  set-face global BufferPadding      $(c $black         default)

  set-face global MatchingChar       $(c default        $black)

  set-face global MenuBackground     $(c $brightblack   $foreground)
  set-face global MenuForeground     $(c default        $brightblack)
  set-face global MenuInfo           $(c $black         default)
  set-face global Information        $(c default        $brightblack)

  set-face global PrimaryCursor      $(c $background    $yellow)
  set-face global PrimaryCursorEol   $(c $background    $brightyellow)
  set-face global PrimarySelection   $(c default        $selection)
  set-face global SecondaryCursor    $(c $background    $foreground)
  set-face global SecondaryCursorEol $(c $background    $brightwhite)
  set-face global SecondarySelection $(c default        $selection)
  set-face global StatusCursor       $(c $background    $yellow)

  set-face global StatusLine         $(c default        default)
  set-face global StatusLineInfo     $(c default        $brightblack)
  set-face global StatusLineMode     $(c default        $brightblack)
  set-face global StatusLineValue    $(c $background    $yellow)
  set-face global Prompt             $(c default        default)

  set-face global Whitespace         $(c default        default)
  set-face global WrapMarker         $(c default        default)

  # Plugins (may need to apply these manually)
  # set-face global PhantomSelection   $(c default        $selection)
  " | tee "$kak_config/colors/challenger-deep-cached.kak"
}}
