# dubious theme

evaluate-commands %sh{
  lightest=rgb:eeeeee
  lighter=rgb:cccccc
  light=rgb:aaaaaa
  dark=rgb:777777
  darker=rgb:555555
  darkest=rgb:111111

  rust=rgb:aa2211
  gold=rgb:aa8800
  pine=rgb:226622
  navy=rgb:1122aa
  violet=rgb:aa44aa

  fg=$darkest
  bg=$lightest

  printf "%s\n" "
  face global attribute          $fg,$bg
  face global block              $fg,$bg
  face global bold               $fg,$bg
  face global builtin            $fg,$bg
  face global bullet             $fg,$bg
  face global comment            $light,$bg
  face global error              $rust,$bg
  face global function           $fg,$bg
  face global header             $fg,$bg
  face global identifier         $fg,$bg
  face global italic             $fg,$bg
  face global keyword            $fg,$bg
  face global link               $fg,$bg
  face global list               $fg,$bg
  face global meta               $fg,$bg
  face global module             $fg,$bg
  face global mono               $fg,$bg
  face global operator           $fg,$bg
  face global string             $gold,$bg
  face global title              $fg,$bg
  face global type               $fg,$bg
  face global value              $fg,$bg
  face global variable           $fg,$bg

  face global Default            $fg,$bg

  face global Error              $rust,$bg

  face global LineNumberCursor   $fg,$bg
  face global LineNumbers        $light,$bg
  face global LineNumbersWrapped $fg,$bg
  face global BufferPadding      $light,$bg

  face global MatchingChar       $fg,$light

  face global MenuBackground     $fg,$lighter
  face global MenuForeground     $bg,$fg
  face global MenuInfo           $fg,$lighter
  face global Information        $fg,$lighter

  face global PrimaryCursor      $bg,$fg
  face global PrimaryCursorEol   $bg,$fg
  face global PrimarySelection   $fg,$light
  face global SecondaryCursor    $bg,$fg
  face global SecondaryCursorEol $bg,$fg
  face global SecondarySelection $fg,$lighter
  face global StatusCursor       $bg,$fg

  face global StatusLine         $fg,$bg
  face global StatusLineInfo     $fg,$lighter
  face global StatusLineMode     $fg,$lighter
  face global StatusLineValue    $fg,$lighter
  face global Prompt             $fg,$bg

  face global Whitespace         $fg,$bg
  face global WrapMarker         $fg,$bg
  "
}
