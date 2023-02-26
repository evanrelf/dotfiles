hook -once global KakBegin .* %{
  # evanrelf/primer.kak
  declare-option str column_color "rgb:f6f8fa"
}

set-option global autowrap_column 80

hook global WinSetOption filetype=rust %{
  set-option window autowrap_column 100
}

hook global WinSetOption filetype=git-commit %{
  add-highlighter window/subject-column column 50 "default,%opt{column_color}"
  set-option window autowrap_column 72
  autowrap-enable
}

hook global WinSetOption autowrap_column=.* %{
  add-highlighter -override window/autowrap-column column \
    %sh{ echo $((kak_opt_autowrap_column + 1)) } \
    "default,%opt{column_color}"
}
