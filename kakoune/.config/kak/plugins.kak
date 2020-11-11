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

plug "h-youhei/kakoune-surround" \
commit "efe74c6f434d1e30eff70d4b0d737f55bf6c5022" config %{
  map global user "s" ": enter-user-mode surround<ret>" -docstring "Surround mode"
  declare-user-mode "surround"
  map global surround "s" ": select-surround<ret>" -docstring "Select surround"
  map global surround "a" ": surround<ret>" -docstring "Add surround"
  map global surround "r" ": change-surround<ret>" -docstring "Replace surround"
  map global surround "d" ": delete-surround<ret>" -docstring "Delete surround"
  map global surround "t" ": enter-user-mode surround-tag<ret>" -docstring "Surround tag"
  declare-user-mode "surround-tag"
  map global surround-tag "t" ": select-surrounding-tag<ret>" -docstring "Select surrounding tag"
  map global surround-tag "a" ": surround-with-tag<ret>" -docstring "Add surrounding tag"
  map global surround-tag "r" ": change-surrounding-tag<ret>" -docstring "Replace surrounding tag"
  map global surround-tag "d" ": delete-surrounding-tag<ret>" -docstring "Delete surrounding tag"
}
