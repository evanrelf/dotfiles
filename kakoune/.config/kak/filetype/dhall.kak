# Better Dhall syntax
source "%val{config}/syntax/dhall2.kak"

# Set filetype to `dhall2` only once (allows switching back to `dhall`
# filetype)
hook global WinCreate .*\.hs %{
  hook -once window WinSetOption filetype=dhall %{
    set-option window filetype dhall2
  }
}

hook global WinSetOption filetype=(dhall|dhall2) %{
  set-option window formatcmd "dhall format --ascii"
}
