# Better Cabal syntax
# Set filetype to `cabal2` only once (allows switching back to `cabal` filetype)
hook global WinCreate .*\.cabal %{
  hook -once window WinSetOption filetype=cabal %{
    set-option window filetype cabal2
  }
}
