source "%val{config}/filetype/haskell.kak"
source "%val{config}/filetype/nix.kak"
source "%val{config}/filetype/elixir.kak"
source "%val{config}/filetype/rust.kak"
source "%val{config}/filetype/purescript.kak"
source "%val{config}/filetype/sh.kak"
source "%val{config}/filetype/fish.kak"
source "%val{config}/filetype/dhall.kak"
source "%val{config}/filetype/python.kak"
source "%val{config}/filetype/markdown.kak"
source "%val{config}/filetype/git-commit.kak"

define-command -docstring "filetype: change filetype" \
filetype -params 1 %{
  set-option window filetype %arg{1}
}

hook -group kak-spec-highlight global BufCreate .*[.](kak-spec) %{
  set-option buffer filetype kak
}
