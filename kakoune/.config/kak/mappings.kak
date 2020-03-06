provide-module "user_mappings" %{

# Use q for backwards word movement
map global normal "q" "b"
map global normal "Q" "B"
map global normal "<a-q>" "<a-b>"
map global normal "<a-Q>" "<a-B>"

# Use caret for macros
map global normal "^" "q"
map global normal "<a-^>" "Q"

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
map global normal "<a-=>" ": format-selections<ret>"

# Allow selecting by line in both directions
map global normal "x" ": extend-line-down %%val{count}<ret>"
map global normal "X" ": extend-line-up %%val{count}<ret>"

# Center viewport when moving through jump list
map global normal "<c-i>" "<c-i>vc"
map global normal "<c-o>" "<c-o>vc"

# Insert and delete spaces for indentation
map global insert "<tab>" "<a-;><a-gt>"
map global insert "<s-tab>" "<a-;><a-lt>"
# hook global InsertChar \t %{
#   try %{
#     execute-keys -draft "hGh<a-k>\A\h+\z<ret><a-;>;%opt{indentwidth}@"
#   } catch %{
#     # execute-keys -draft "<esc>h%opt{indentwidth}@"
#     execute-keys -draft "<esc>hd>"
#   }
# }
hook global InsertDelete ' ' %{ try %{
  execute-keys -draft 'hGh<a-k>\A\h+\z<ret>i<space><esc><lt>'
}}

# Tags
map global user "t" "<a-i>w: ctags-search<ret>;<space>" -docstring "Jump to tag under cursor"

# User mode
evaluate-commands %sh{
  case "$(uname)" in
    "Darwin")
      copy="pbcopy"
      paste="pbpaste"
      ;;
    "Linux")
      copy="xclip"
      paste="xclip -o"
      ;;
    *)
      copy="false"
      paste="false"
      ;;
  esac
  printf "%s" "
  map global user 'y' '<a-|>$copy<ret>' -docstring 'Yank to clipboard'
  map global user 'p' '<a-!>$paste<ret>' -docstring 'Paste after from clipboard'
  map global user 'P' '!$paste<ret>' -docstring 'Paste before from clipboard'
  map global user 'R' '|$paste<ret>' -docstring 'Paste replace from clipboard'
  "
}
map global user "/" ": execute-keys /<ret>\Q\E<left><left>" -docstring "Search without regex"
map global user "=" ": format-buffer<ret>" -docstring "Format buffer"
declare-user-mode filetype
map global user "<space>" ": enter-user-mode filetype<ret>" -docstring "Filetype mode"

# Disabled
map global normal "<a-h>" ": echo -markup '{Error}Use Gh{Default}'<ret>" -docstring "Use Gh"
map global normal "<a-l>" ": echo -markup '{Error}Use Gl{Default}'<ret>" -docstring "Use Gl"
map global goto "g" "<esc>: echo -markup '{Error}Use gk{Default}'<ret>" -docstring "Use gk"
map global view "v" "<esc>: echo -markup '{Error}Use vc{Default}'<ret>" -docstring "Use vc"
map global normal "b" ": echo -markup '{Error}Use q{Default}'<ret>" -docstring "Use q"
map global normal "B" ": echo -markup '{Error}Use Q{Default}'<ret>" -docstring "Use Q"
map global normal "<a-b>" ": echo -markup '{Error}Use ⌥q{Default}'<ret>" -docstring "Use ⌥q"
map global normal "<a-B>" ": echo -markup '{Error}Use ⌥Q{Default}'<ret>" -docstring "Use ⌥Q"

}
