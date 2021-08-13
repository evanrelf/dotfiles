hook global WinSetOption filetype=zig %{
  set-option window formatcmd "zig fmt --stdin"
}
