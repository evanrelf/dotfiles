# Use q for backwards word movement
map global "normal" "q" "b"
map global "normal" "Q" "B"
map global "normal" "<a-q>" "<a-b>"
map global "normal" "<a-Q>" "<a-B>"
map global "normal" "b" "q"
map global "normal" "B" "Q"
map global "normal" "<a-b>" ": fail 'Use a-q'<ret>" -docstring "Use a-q"
map global "normal" "<a-B>" ": fail 'Use a-Q'<ret>" -docstring "Use a-Q"

# Drop selection when going into insert mode
map global "normal" "i" ";i"
map global "normal" "a" ";a"

# Comment out line
map global "normal" "#" ": comment-line<ret>"

# Clear search register
map global "normal" "<esc>" ": set-register / ''<ret>: execute-keys HL<ret>"

# Use space as leader
map global "normal" "," "<space>"
map global "normal" "<space>" ","
map global "normal" "<a-,>" "<a-space>"
map global "normal" "<a-space>" "<a-,>"

# Buffer
declare-user-mode "buffer"
map global "user" "b" ": enter-user-mode buffer<ret>" -docstring "Buffer"
map global "buffer" "n" ": buffer-next<ret>" -docstring "Next buffer"
map global "buffer" "p" ": buffer-previous<ret>" -docstring "Previous buffer"
map global "buffer" "d" ": delete-buffer<ret>" -docstring "Delete buffer"

# File
declare-user-mode "file"
map global "user" "f" ": enter-user-mode file<ret>" -docstring "File"
map global "file" "s" ": user-file-save<ret>" -docstring "Save file"

# Quit
declare-user-mode "quit"
map global "user" "q" ": enter-user-mode quit<ret>" -docstring "Quit"
map global "quit" "q" ": quit<ret>" -docstring "Quit"

# Discourage non-mnemonic keys
map global "normal" "<a-h>" ": fail 'Use Gh'<ret>" -docstring "Use Gh"
map global "normal" "<a-l>" ": fail 'Use Gl'<ret>" -docstring "Use Gl"
map global "normal" "<a-H>" ": fail 'Use Gh'<ret>" -docstring "Use Gh"
map global "normal" "<a-L>" ": fail 'Use Gl'<ret>" -docstring "Use Gl"
map global "goto" "g" "<esc>: fail 'Use gk'<ret>" -docstring "Use gk"
map global "view" "v" "<esc>: fail 'Use vc'<ret>" -docstring "Use vc"
map global "insert" "<c-n>" "<a-;>: fail 'Use tab'<ret>" -docstring "Use tab"
map global "insert" "<c-p>" "<a-;>: fail 'Use s-tab'<ret>" -docstring "Use s-tab"

define-command -hidden user-file-save %{
  try %{ evaluate-commands %sh{
    if [ "$kak_modified" = "true" ]; then
      echo "write"
      if command -v grealpath >/dev/null 2>&1; then
        relative_path=$(grealpath --relative-to="$(pwd)" "$kak_buffile")
        echo "echo 'Saved $relative_path'"
      else
        echo "echo 'Saved $kak_buffile'"
      fi
    fi
  }} catch %{
    fail "Failed to save %val{buffile}"
  }
}
