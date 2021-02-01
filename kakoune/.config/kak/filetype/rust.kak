hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --edition 2018 --emit stdout"
  hook window BufWritePre .* %{ format-buffer }
}
