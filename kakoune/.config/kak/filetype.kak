source "%val{config}/filetype/haskell.kak"
source "%val{config}/filetype/nix.kak"
source "%val{config}/filetype/sh.kak"
source "%val{config}/filetype/purescript.kak"
source "%val{config}/filetype/fish.kak"
source "%val{config}/filetype/dhall.kak"
source "%val{config}/filetype/rust.kak"
source "%val{config}/filetype/git-commit.kak"

define-command -docstring "filetype: change filetype" \
filetype -params 1 %{
  set-option window filetype %arg{1}
}
