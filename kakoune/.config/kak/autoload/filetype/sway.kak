hook global BufCreate .*/\.config/sway/.+ %{
  set buffer filetype i3
}
