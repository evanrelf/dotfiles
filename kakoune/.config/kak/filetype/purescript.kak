# Better PureScript syntax
source "%val{config}/syntax/purescript2.kak"

# Set filetype to `purescript2` only once (allows switching back to `purescript`
# filetype)
hook global WinCreate .*\.purs %{
  hook -once window WinSetOption filetype=purescript %{
    set-option window filetype purescript2
  }
}
