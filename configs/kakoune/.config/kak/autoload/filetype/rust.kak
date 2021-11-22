hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --edition 2018 --emit stdout"
  hook window BufWritePre .* %{ format-buffer }
  set-option window autowrap_column 101
  add-highlighter window/rust-column column 81 "default,%opt{column_color}"
}
