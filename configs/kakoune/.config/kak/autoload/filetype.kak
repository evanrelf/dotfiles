define-command -docstring "filetype: change filetype" \
filetype -params 1 %{
  set-option window filetype %arg{1}
}

hook -group kak-spec-highlight global BufCreate .*[.](kak-spec) %{
  set-option buffer filetype kak
}
