hook -group kak-spec-highlight global BufCreate .*[.](kak-spec) %{
  set-option buffer filetype kak
}
