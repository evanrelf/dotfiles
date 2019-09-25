# Normal
map global normal "," "<space>"
map global normal "<space>" ","
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"
map global normal "#" ": comment-line<ret>"
map global normal "<a-#>" ": comment-block<ret>"

# Insert
map global insert "<tab>" "<a-;><gt>"
map global insert "<s-tab>" "<a-;><lt>"

# User
map global user "y" "<a-|>pbcopy<ret>" -docstring "Yank to system clipboard"
map global user "p" "<a-!>pbpaste<ret>" -docstring "Paste from system clipboard after selection"
map global user "P" "!pbpaste<ret>" -docstring "Paste from system clipboard before selection"

# Escape with jk
hook global InsertChar "k" %{ try %{
  execute-keys -draft "hH <a-k>jk<ret> d"
  execute-keys "<esc>"
}}

# Disabled
map global goto "g" "<esc>" -docstring "Use gk"
