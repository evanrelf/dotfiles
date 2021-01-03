declare-user-mode "markdown"

hook global WinSetOption filetype=markdown %{
  map window "user" "," ": enter-user-mode markdown<ret>" -docstring "Markdown..."
  map window "markdown" "f" "|fmt -w80<ret>" -docstring "Format"
}
