hook global WinSetOption filetype=zig %{
  set-option window indentwidth 4
  set-option window formatcmd "zig fmt --stdin"
  hook window BufWritePre .* %{ format-buffer }
}
