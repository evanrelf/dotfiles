provide-module "user_mappings" %{

# Space is my leader
map global normal "," "<space>"
map global normal "<space>" ","
map global normal "<a-,>" "<a-space>"
map global normal "<a-space>" "<a-,>"

# Make search case-insensitive by default
map global normal "/" "/(?i)"
map global normal "?" "?(?i)"
map global normal "<a-/>" "<a-/>(?i)"
map global normal "<a-?>" "<a-?>(?i)"

# Use alternative registers for K and S stuff
map global normal "<a-k>" '"k<a-k>'
map global normal "<a-K>" '"k<a-K>'
map global normal "s" '"ss'
map global normal "S" '"sS'
map global normal "<a-s>" '"s<a-s>'
map global normal "<a-S>" '"s<a-S>'

# Commenting
map global normal "#" ": comment-line<ret>"
map global normal "<a-#>" ": comment-block<ret>"

# Formatting
map global normal "=" ": format-buffer<ret>"
map global normal "<a-=>" ": format-selections<ret>"

# Allow selecting by line in both directions
map global normal "x" ": extend-line-down %%val{count}<ret>"
map global normal "X" ": extend-line-up %%val{count}<ret>"

# Move viewport
map global normal "<left>" "vh"
map global normal "<down>" "vj"
map global normal "<up>" "vk"
map global normal "<right>" "vl"
map global insert "<left>" "<a-;>vh"
map global insert "<down>" "<a-;>vj"
map global insert "<up>" "<a-;>vk"
map global insert "<right>" "<a-;>vl"

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
map global user "=" ":prompt math: %%{exec 'a%%val{text}<lt>esc>|bc<lt>ret>'}<ret>" -docstring "Insert calculation"
declare-user-mode filetype
map global user "<space>" ": enter-user-mode filetype<ret>" -docstring "Filetype mode"

# # Escape with jk
# hook global InsertChar "k" %{ try %{
#   execute-keys -draft "hH <a-k>jk<ret> d"
#   execute-keys "<esc>"
# }}

# Disabled
map global normal "<a-h>" ": echo -markup '{Error}Use Gh{Default}'<ret>" -docstring "Use Gh"
map global normal "<a-l>" ": echo -markup '{Error}Use Gl{Default}'<ret>" -docstring "Use Gl"
map global goto "g" "<esc>: echo -markup '{Error}Use gk{Default}'<ret>" -docstring "Use gk"
map global view "v" "<esc>: echo -markup '{Error}Use vc{Default}'<ret>" -docstring "Use vc"

}
