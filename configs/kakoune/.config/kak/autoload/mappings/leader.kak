# Use space as leader
map global "normal" "," "<space>"
map global "normal" "<space>" ","
map global "normal" "<a-,>" "<a-space>"
map global "normal" "<a-space>" "<a-,>"

# Uncategorized
map global "user" "`" "ga" -docstring "Last buffer"

# Buffer
declare-user-mode "buffer"
map global "user" "b" ": enter-user-mode buffer<ret>" -docstring "Buffer..."
map global "buffer" "n" ": buffer-next<ret>" -docstring "Next buffer"
map global "buffer" "p" ": buffer-previous<ret>" -docstring "Previous buffer"
map global "buffer" "d" ": delete-buffer<ret>" -docstring "Delete buffer"
map global "buffer" "u" ": buffer *debug*<ret>" -docstring "Go to Debug buffer"
map global "buffer" "=" ": format-buffer<ret>" -docstring "Format buffer"

# File
declare-user-mode "file"
map global "user" "f" ": enter-user-mode file<ret>" -docstring "File..."
map global "file" "s" ": user-file-save<ret>" -docstring "Save file"

define-command -hidden user-file-save %{
  try %{ evaluate-commands %sh{
    if [ "${kak_modified}" = "true" ]; then
      echo "write"
      if command -v grealpath >/dev/null 2>&1; then
        file_path=$(grealpath --relative-to="$(pwd)" "${kak_buffile}")
      elif command -v realpath >/dev/null 2>&1; then
        file_path=$(realpath --relative-to="$(pwd)" "${kak_buffile}")
      else
        file_path="${kak_buffile}"
      fi
      echo "echo 'Saved \"${file_path}\"'"
    fi
  }} catch %{
    fail "Failed to save %val{buffile}"
  }
  git update-diff
}
