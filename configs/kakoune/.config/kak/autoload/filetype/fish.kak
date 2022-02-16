hook global WinSetOption filetype=fish %{
  set-option window indentwidth 4
  set-option window formatcmd "fish_indent"
  hook window BufWritePre .* %{ format-buffer }
}
