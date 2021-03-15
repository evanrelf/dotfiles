hook global WinSetOption filetype=elixir %{
  set-option window formatcmd "mix format -"
  # hook window BufWritePre .* %{ format-buffer }
}
