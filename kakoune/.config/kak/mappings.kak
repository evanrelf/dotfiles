# Normal

# Space is my leader
map global normal "," "<space>"
map global normal "<space>" ","

# Make search case-insensitive
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"

# Commenting
map global normal "#" ": comment-line<ret>"
map global normal "<a-#>" ": comment-block<ret>"

# Center viewport when moving between search results
# map global normal "n" "nvc"
# map global normal "<a-n>" "<a-n>vc"

# Change indentation in insert mode with Tab
map global insert "<tab>" "<a-;><gt>"
map global insert "<s-tab>" "<a-;><lt>"

# User mode
map global user "y" "<a-|>pbcopy<ret>" -docstring "Yank to system clipboard"
map global user "p" "<a-!>pbpaste<ret>" -docstring "Paste from system clipboard after selection"
map global user "P" "!pbpaste<ret>" -docstring "Paste from system clipboard before selection"

# Escape with jk
hook global InsertChar "k" %{ try %{
  execute-keys -draft "hH <a-k>jk<ret> d"
  execute-keys "<esc>"
}}

# Disabled
map global normal "<a-h>" ": echo 'Use Gh'<ret>" -docstring "Use Gh"
map global normal "<a-l>" ": echo 'Use Gl'<ret>" -docstring "Use Gl"
map global goto "g" ": echo 'Use gk'<ret>" -docstring "Use gk"
map global view "v" ": echo 'Use vc'<ret>" -docstring "Use vc"
