source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "robertmeta/plug.kak" noload

plug "evanrelf/primer.kak" theme config %{
  colorscheme primer

  add-highlighter global/wrap-column column %sh{ echo $((kak_opt_autowrap_column + 1)) } default,rgb:f6f8fa

  hook global WinSetOption filetype=git-commit %{
    remove-highlighter global/wrap-column
    add-highlighter window/subject-column column 51 default,rgb:f6f8fa
    add-highlighter window/body-column column 73 default,rgb:f6f8fa
  }
}

plug "evanrelf/expand-line.kak"

# plug "evanrelf/number-toggle.kak" branch "override-highlighter" config %{
#   set-option global number_toggle_params -hlcursor
# }
