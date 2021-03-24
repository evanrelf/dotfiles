# Better Nix syntax
# Set filetype to `nix2` only once (allows switching back to `nix`
# filetype)
hook global WinCreate .*\.nix %{
  hook -once window WinSetOption filetype=nix %{
    set-option window filetype nix2
  }
}
