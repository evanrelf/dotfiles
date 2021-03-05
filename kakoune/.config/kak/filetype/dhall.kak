# Better Dhall syntax
source "%val{config}/syntax/dhall2.kak"

# Set filetype to `dhall2` only once (allows switching back to `dhall`
# filetype)
hook global WinCreate .*\.dhall %{
  hook -once window WinSetOption filetype=dhall %{
    set-option window filetype dhall2
  }
}

hook global WinSetOption filetype=(dhall|dhall2) %{
  set-option window formatcmd "dhall format --ascii"
  map window "user" "," ": enter-user-mode haskell<ret>" -docstring "Haskell..."
  map window "haskell" "f" "|dhall format --ascii<ret>" -docstring "Format (ASCII)"
  map window "haskell" "F" "|dhall format<ret>" -docstring "Format (Unicode)"
  # hook window BufWritePre .* %{ format-buffer }
}
