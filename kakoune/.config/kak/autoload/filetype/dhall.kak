declare-user-mode "dhall"

# Better Dhall syntax
# Set filetype to `dhall2` only once (allows switching back to `dhall`
# filetype)
hook global WinCreate .*\.dhall %{
  hook -once window WinSetOption filetype=dhall %{
    set-option window filetype dhall2
  }
}

hook global WinSetOption filetype=(dhall|dhall2) %{
  set-option window formatcmd "dhall format --ascii"
  map window "user" "," ": enter-user-mode dhall<ret>" -docstring "Dhall..."
  map window "dhall" "f" "|dhall format --ascii<ret>" -docstring "Format (ASCII)"
  map window "dhall" "F" "|dhall format<ret>" -docstring "Format (Unicode)"
}
