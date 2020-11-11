hook global WinSetOption filetype=rust %{
  set-option window indentwidth 4
  set-option window formatcmd "rustfmt --emit stdout"
  hook window -group format BufWritePre .* %{ format-buffer }
}
