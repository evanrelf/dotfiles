# evanrelf theme

evaluate-commands %sh{
  white=rgb:dddddd
  black=rgb:000000
  gray=rgb:999999

  red=rgb:ff7777
  orange=rgb:ffaa55
  yellow=rgb:eeee77
  green=rgb:99ee99
  blue=rgb:7799ff
  purple=rgb:dd88dd

  printf "%s\n" "
  face global attribute          default,default
  face global block              default,default
  face global bold               default,default
  face global builtin            default,default
  face global bullet             default,default
  face global comment            $gray,default
  face global error              $red,default
  face global function           default,default
  face global header             default,default
  face global identifier         default,default
  face global italic             default,default
  face global keyword            default,default
  face global link               default,default
  face global list               default,default
  face global meta               default,default
  face global module             default,default
  face global mono               default,default
  face global operator           $blue,default
  face global string             $yellow,default
  face global title              default,default
  face global type               default,default
  face global value              default,default
  face global variable           default,default

  face global Default            $white,$black

  face global Error              $black,$red
  face global Information        default,default

  face global LineNumberCursor   default,default
  face global LineNumbers        $gray,default
  face global LineNumbersWrapped default,default
  face global BufferPadding      default,default

  face global MatchingChar       default,$gray

  face global MenuBackground     default,default
  face global MenuForeground     $black,$white
  face global MenuInfo           default,default

  face global PrimaryCursor      $black,$white
  face global PrimaryCursorEol   $black,$white
  face global PrimarySelection   default,$gray

  face global SecondaryCursor    $black,$white
  face global SecondaryCursorEol $black,$white
  face global SecondarySelection default,$gray

  face global StatusCursor       $black,$white
  face global StatusLine         default,default
  face global StatusLineInfo     default,default
  face global StatusLineMode     default,default
  face global StatusLineValue    default,default

  face global Prompt             $green,default

  face global Whitespace         default,default
  face global WrapMarker         default,default
  "
}
