hook global BufCreate .*\.p8 %{
  set buffer filetype lua
}
