# Space is my leader
map global normal "," "<space>"
map global normal "<space>" ","
map global normal "<a-,>" "<a-space>"
map global normal "<a-space>" "<a-,>"

# Make search case-insensitive
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"

# Commenting
map global normal "#" ": comment-line<ret>"
map global normal "<a-#>" ": comment-block<ret>"

# Allow selecting by line in both directions
map global normal "x" ": extend-line-down %%val{count}<ret>"
map global normal "X" ": extend-line-up %%val{count}<ret>"

# Center viewport when moving between search results
map global normal "n" "nvc"
map global normal "N" "Nvc"
map global normal "<a-n>" "<a-n>vc"
map global normal "<a-N>" "<a-N>vc"

# Insert and delete spaces for indentation
hook global InsertChar \t %{ try %{
  execute-keys -draft "h<a-h><a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@"
}}
hook global InsertDelete ' ' %{ try %{
  execute-keys -draft 'h<a-h><a-k>\A\h+\z<ret>i<space><esc><lt>'
}}

# User mode
map global user "y" "<a-|>pbcopy<ret>" -docstring "Yank to clipboard"
map global user "p" "<a-!>pbpaste<ret>" -docstring "Paste after from clipboard"
map global user "P" "!pbpaste<ret>" -docstring "Paste before from clipboard"
map global user "R" "|pbpaste<ret>" -docstring "Paste replace from clipboard"
map global user "/" ": execute-keys /<ret>\Q\E<left><left>" -docstring "Search without regex"
declare-user-mode filetype
map global user "<space>" ": enter-user-mode filetype<ret>" -docstring "Filetype mode"
map global normal "=" ":prompt math: %%{exec 'a%%val{text}<lt>esc>|bc<lt>ret>'}<ret>"

# Escape with jk
hook global InsertChar "k" %{ try %{
  execute-keys -draft "hH <a-k>jk<ret> d"
  execute-keys "<esc>"
}}

# Disabled
map global normal "<a-h>" ": echo -markup '{Error}Use Gh{Default}'<ret>" -docstring "Use Gh"
map global normal "<a-l>" ": echo -markup '{Error}Use Gl{Default}'<ret>" -docstring "Use Gl"
map global goto "g" "<esc>: echo -markup '{Error}Use gk{Default}'<ret>" -docstring "Use gk"
map global view "v" "<esc>: echo -markup '{Error}Use vc{Default}'<ret>" -docstring "Use vc"
