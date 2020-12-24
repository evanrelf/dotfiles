# Use space as leader
map global "normal" "," "<space>"
map global "normal" "<space>" ","
map global "normal" "<a-,>" "<a-space>"
map global "normal" "<a-space>" "<a-,>"

# Uncategorized
map global "user" "`" "ga" -docstring "Last buffer"

# Search
declare-user-mode "search"
map global "user" "/" ": enter-user-mode search<ret>" -docstring "Search..."
map global "search" "g" ": grep " -docstring "grep"
map global "search" "q" ": execute-keys /<ret>\Q\E<left><left>" -docstring "Search without regex"

# Buffer
declare-user-mode "buffer"
map global "user" "b" ": enter-user-mode buffer<ret>" -docstring "Buffer..."
map global "buffer" "n" ": buffer-next<ret>" -docstring "Next buffer"
map global "buffer" "p" ": buffer-previous<ret>" -docstring "Previous buffer"
map global "buffer" "d" ": delete-buffer<ret>" -docstring "Delete buffer"
map global "buffer" "f" ": format-buffer<ret>" -docstring "Format buffer"
map global "buffer" "r" ": buffer *grep*<ret>" -docstring "Go to grep buffer"
map global "buffer" "u" ": buffer *debug*<ret>" -docstring "Go to Debug buffer"
try %{
  require-module "fzf"
  map global "buffer" "b" ": fzf-buffer<ret>" -docstring "Switch buffer"
}

# File
declare-user-mode "file"
map global "user" "f" ": enter-user-mode file<ret>" -docstring "File..."
map global "file" "s" ": user-file-save<ret>" -docstring "Save file"
try %{
  require-module "fzf"
  map global "file" "f" ": fzf-file<ret>" -docstring "Find file"
}

# Git
declare-user-mode "git"
map global "user" "g" ": enter-user-mode git<ret>" -docstring "Git..."
try %{
  require-module "fzf"
  require-module "fzf_vcs"
  map global "git" "f" ": fzf-vcs<ret>" -docstring "Find file"
}

define-command -hidden user-file-save %{
  try %{ evaluate-commands %sh{
    if [ "$kak_modified" = "true" ]; then
      echo "write"
      if command -v grealpath >/dev/null 2>&1; then
        file_path=$(grealpath --relative-to="$(pwd)" "$kak_buffile")
      elif command -v realpath >/dev/null 2>&1; then
        file_path=$(realpath --relative-to="$(pwd)" "$kak_buffile")
      else
        file_path="$kak_buffile"
      fi
      echo "echo 'Saved \"$file_path\"'"
    fi
  }} catch %{
    fail "Failed to save %val{buffile}"
  }
}

# Lint
declare-user-mode "lint"
map global "user" "l" ": enter-user-mode lint<ret>" -docstring "Lint..."
map global "lint" "l" ": lint-buffer<ret>" -docstring "Lint"
map global "lint" "n" ": lint-next-message<ret>" -docstring "Next message"
map global "lint" "p" ": lint-previous-message<ret>" -docstring "Previous message"

# Quit
declare-user-mode "quit"
map global "user" "q" ": enter-user-mode quit<ret>" -docstring "Quit..."
map global "quit" "q" ": client-quit<ret>" -docstring "Quit"
map global "quit" "s" ": write<ret>: client-quit<ret>" -docstring "Save and quit"
map global "quit" "k" ": kill<ret>" -docstring "Kill daemon"
