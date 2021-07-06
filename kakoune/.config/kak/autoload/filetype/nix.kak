declare-user-mode "nix"

# Better Nix syntax
# Set filetype to `nix2` only once (allows switching back to `nix`
# filetype)
hook global WinCreate .*\.nix %{
  hook -once window WinSetOption filetype=nix %{
    set-option window filetype nix2
  }
}

hook global WinSetOption filetype=(nix|nix2) %{
  set-option window formatcmd "nixpkgs-fmt"
  map window "user" "," ": enter-user-mode nix<ret>" -docstring "Nix..."
  map window "nix" "h" "i0000000000000000000000000000000000000000000000000000<esc>" -docstring "Insert dummy hash"
}
