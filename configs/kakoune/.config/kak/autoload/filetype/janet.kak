hook global WinSetOption filetype=janet %{
  set-option window formatcmd "janetfmt"
  hook window BufWritePre .* %{ format-buffer }
}
