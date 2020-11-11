hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --emit stdout"
  hook window BufWritePre .* %{ format-buffer }
}
