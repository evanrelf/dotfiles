hook global WinSetOption filetype=git-commit %{
  set-option window autowrap_column 72
  autowrap-enable
  add-highlighter window/subject-column column 51 "default,%opt{column_color}"
}
