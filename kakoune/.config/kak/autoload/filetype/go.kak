hook global WinSetOption filetype=go %{
  set-option window indentwidth 0
  set-option window tabstop 2
  set-option window formatcmd "gofmt"
  hook window BufWritePre .* %{ format-buffer }
}
