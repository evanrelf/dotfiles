# Normal
# map global normal ";" ":"
# map global normal ":" ";"
map global normal "," "<space>"
map global normal "<space>" ","
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"
map global normal "#" ": comment-line<ret>"

# User
map global user "y" "<a-|>pbcopy<ret>" -docstring "Yank to system clipboard"
map global user "p" "!pbpaste<ret>" -docstring "Paste from system clipboard"
map global user "=" ": format<ret>" -docstring "Format buffer"
map global user "f" ": fzf-mode<ret>" -docstring "FZF mode"

# Escape with jk
hook global InsertChar k %{ try %{
  execute-keys -draft hH <a-k>jk<ret> d
  execute-keys <esc>
}}

# Disable
map global goto "g" "<esc>" -docstring "Use gk"
